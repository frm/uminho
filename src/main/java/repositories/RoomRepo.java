package repositories;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import communication.Msg;
import models.Room;
import notification.Notification;

import java.util.HashMap;

/**
 * Created by joaorodrigues on 30 Nov 15.
 */
public class RoomRepo extends BasicActor<Msg, Void> {
    private HashMap<String, ActorRef<Msg>> rooms;
    private ActorRef notificationHandler;

    public RoomRepo(ActorRef nh){
        rooms = new HashMap<>();
        this.notificationHandler = nh;
    }

    public ActorRef getRoom(String name){
        return rooms.get(name);
    }

    private boolean closeRoom(String name, ActorRef requester) throws SuspendExecution {
        ActorRef room = rooms.get(name);

        if(room != null)
            room.send( new Msg(Msg.Type.CLOSE, null, self()));
        else
            return false;

        return true;


    }

    private boolean createRoom(String name){
        if(rooms.containsKey(name))
            return false;

        Room room = new Room(notificationHandler, name);

        room.spawn();

        rooms.put(name, room.ref());

        return true;

    }

    private Msg joinRoom(String room, String username, ActorRef<Msg> sender) throws SuspendExecution {
        ActorRef<Msg> r = rooms.get(room);
        if(r != null)
            r.send(new Msg(Msg.Type.JOIN, username, sender));

        return new Msg(Msg.Type.ROOM, r, self());
    }

    public void addRoom(Room room, String name){
        rooms.put(name, room.ref());
    }
    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {
        // TODO: Remove this when we have admin
        createRoom("default");
        while(
            receive(msg -> {
                ActorRef<Msg> sender = msg.sender;
                String[] args = (String[])msg.content;
                switch (msg.type) {
                    case JOIN:
                        sender.send( joinRoom(args[0], args[1], sender) );
                        return true;
                    case GET_ROOMS:
                        sender.send( new Msg(Msg.Type.ROOMS, rooms.keySet().toString(), self()));
                        return true;
                    case ADD:
                        if( createRoom((String) msg.content ) ) {
                            sender.send(new Msg(Msg.Type.OK, null, self()));
                            notificationHandler.send( new Notification(Notification.Type.CREATE, (String) msg.content));
                        }
                        return true;
                    case REMOVE:
                        if( closeRoom( (String) msg.content, sender) ) {
                            sender.send(new Msg(Msg.Type.OK, null, self()));
                            notificationHandler.send( new Notification(Notification.Type.REMOVE, (String) msg.content));
                        }
                        return true;
                }

                return false;
        }));
        return null;
    }

}
