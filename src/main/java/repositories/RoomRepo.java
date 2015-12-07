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

    private void sendTo(ActorRef<Msg> target, Msg.Type type, Object attachment) throws SuspendExecution {
        target.send(new Msg(type, attachment, self()));
    }

    public ActorRef getRoom(String name){
        return rooms.get(name);
    }

    private boolean closeRoom(String name, ActorRef requester) throws SuspendExecution {
        ActorRef room = rooms.get(name);
        boolean b = room != null;

        if(b)
            sendTo(room, Msg.Type.CLOSE, null);

        return b;
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

    private void sendRooms(ActorRef<Msg> sender) throws SuspendExecution {
        sendTo(sender, Msg.Type.ROOMS, new String[] { rooms.keySet().toString() });
    }

    public void addRoom(Room room, String name){
        rooms.put(name, room.ref());
    }
    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {
        // TODO: Remove this when we have admin
        createRoom("default");
        createRoom("general");
        while(
            receive(msg -> {
                ActorRef<Msg> sender = msg.sender;
                String[] args = (String[])msg.content;
                switch (msg.type) {
                    case JOIN:
                        sender.send( joinRoom(args[0], args[1], sender) );
                        return true;
                    case GET_ROOMS:
                        sendRooms(sender);
                        return true;
                    case ADD:
                        if( createRoom((String) msg.content ) ) {
                            sendTo(sender, Msg.Type.OK, null);
                            notificationHandler.send( new Notification(Notification.Type.CREATE, (String) msg.content));
                        }
                        return true;
                    case REMOVE:
                        if( closeRoom( (String) msg.content, sender) ) {
                            sendTo(sender, Msg.Type.OK, null);
                            notificationHandler.send( new Notification(Notification.Type.REMOVE, (String) msg.content));
                        }
                        return true;
                }

                return false;
        }));
        return null;
    }

}
