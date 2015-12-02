package server;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import util.Msg;

/**
 * Created by joaorodrigues on 2 Dec 15.
 */
public class MessageHandler extends BasicActor<Msg, Void> {
    private ActorRef currRoom;
    private ActorRef user;
    private ActorRef roomRepo;
    private String username;


    public MessageHandler(ActorRef user, ActorRef roomRepo, String username){
        currRoom = null;
        this.user = user;
        this.roomRepo = roomRepo;
        this.username = username;
    }

    private void joinRoom() throws SuspendExecution, InterruptedException {


        if(currRoom == null)
            user.send( new Msg(Msg.Type.ERROR, null, self()));
        else
            currRoom.send( new Msg(Msg.Type.ADD, username, self()));
    }

    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {


        while(
            receive(msg -> {
                ActorRef sender = msg.sender;
                switch (msg.type) {
                    //FROM LINEREADER
                    case PM:
                        //TODO: Handle Private Messages (take into account the receiver)
                        return true;
                    case CHAT:
                        currRoom.send(msg);
                        return true;
                    case JOIN:
                        roomRepo.send( new Msg(Msg.Type.GET_ROOM, (String) msg.content, self()));
                        return true;
                    case LEAVE:
                        currRoom.send( new Msg(Msg.Type.REMOVE,username, self()));
                        return true;
                    case GET_ROOM_USERS:
                        currRoom.send( new Msg(Msg.Type.GET_ROOM_USERS, null, self()));
                        return true;
                    case GET_ROOMS:
                        roomRepo.send( new Msg(Msg.Type.GET_ROOMS, null, self()));
                    //FROM ROOM REPO
                    case ROOM:
                        currRoom = (ActorRef) msg.content;
                        joinRoom();
                        return true;
                    case ROOMS:
                        user.send(msg);
                        return true;
                    //FROM ROOM
                    case NEW_CHAT:
                        user.send(msg);
                        return true;
                    case ROOM_USERS:
                        user.send(msg);
                        return true;
                    case KICK:
                        currRoom = null;
                        user.send(msg);
                        return true;
                    case OK:
                        user.send(msg);
                        return true;
                }

                return false;
            }));

        return null;
    }
}
