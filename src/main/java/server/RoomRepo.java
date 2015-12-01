package server;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import util.Msg;

import java.util.HashMap;

/**
 * Created by joaorodrigues on 30 Nov 15.
 */
public class RoomRepo extends BasicActor<Msg, Void> {
    private HashMap<String, ActorRef> rooms;

    public RoomRepo(){
        rooms = new HashMap<>();

    }

    private ActorRef getRoom(String name){
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

        Room room = new Room();

        room.spawn();

        rooms.put(name, room.ref());

        return true;

    }

    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {
        receive(msg -> {
            ActorRef sender = msg.getSender();
            switch (msg.getType()) {
                case ADD:
                    if( createRoom((String) msg.getContent() ) )
                        sender.send( new Msg(Msg.Type.OK, null, self()));
                    else
                        sender.send( new Msg(Msg.Type.ERROR, null, self()));
                    return true;
                case REMOVE:
                    if( closeRoom( (String) msg.getContent(), sender) )
                        sender.send( new Msg(Msg.Type.OK, null, self()));
                    else
                        sender.send( new Msg(Msg.Type.ERROR, null, self()));
                    return true;
                case REF:
                    ActorRef ref = getRoom( (String) msg.getContent());
                    if(ref != null)
                        sender.send( new Msg(Msg.Type.OK, ref, self()));
                    else
                        sender.send( new Msg(Msg.Type.ERROR, null, self()));
            }

            return false;
        });
        return null;
    }

}
