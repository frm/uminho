package repositories;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import communication.Msg;
import models.Room;
import notification.Notification;
import util.MessageBuilder;

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

    private boolean closeRoom(String name) throws SuspendExecution {
        ActorRef<Msg> room = rooms.get(name);
        boolean b = room != null;

        if(b) {
            sendTo(room, Msg.Type.CLOSE, null);
            rooms.remove(name);
        }

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
        // STOP. HAMMER TIME
        sendTo(sender, Msg.Type.ROOMS, new String[] { rooms.keySet().toString() + "\n" });
    }

    private void addRoom(String[] args, ActorRef<Msg> sender) throws SuspendExecution {
        String reply;
        Msg.Type t;
        if( createRoom(args[0])) {
            reply = MessageBuilder.message(MessageBuilder.CREATE_SUCCESS);
            t = Msg.Type.OK;
            notificationHandler.send( new Notification(Notification.Type.CREATE, args[0] ));
        }
        else {
            t = Msg.Type.ERROR;
            reply = MessageBuilder.message(MessageBuilder.CREATE_INVALID);
        }

        sendTo(sender, t, new String[] { reply });
    }

    private void removeRoom(String[] args, ActorRef<Msg> sender) throws SuspendExecution {
        String reply;
        Msg.Type t;
        if( closeRoom(args[0]) ) {
            reply = MessageBuilder.message(MessageBuilder.REMOVE_SUCCESS);
            t = Msg.Type.OK;
            notificationHandler.send( new Notification(Notification.Type.REMOVE, args[0]));
        }
        else {
            t = Msg.Type.ERROR;
            reply = MessageBuilder.message(MessageBuilder.REMOVE_INVALID);
        }

        sendTo(sender, t, new String[] { reply });
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
                        addRoom(args, sender);
                        return true;
                    case REMOVE:
                        removeRoom(args, sender);
                        return true;
                }

                return false;
        }));
        return null;
    }
}
