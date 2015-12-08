package models;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import communication.Msg;
import notification.Notification;
import util.MessageBuilder;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;


/**
 * Created by joaorodrigues on 30 Nov 15.
 */
public class Room extends BasicActor<Msg, Void> {
    private HashMap<String, ActorRef<Msg>> members;
    private ActorRef notificationHandler;
    private String name;

    public Room(ActorRef nh, String name) {
        members = new HashMap<>();
        this.notificationHandler = nh;
        this.name = name;
    }

    private void sendTo(ActorRef<Msg> target, Msg.Type type, Object attachment) throws SuspendExecution {
        target.send(new Msg(type, attachment, self()));
    }

    private void kickMembers() throws SuspendExecution {
        for (ActorRef<Msg> member : members.values()) {
            sendTo(member, Msg.Type.KICK, null);
        }
    }

    private boolean addMember(String name, ActorRef<Msg> ref) {
        if (!members.containsKey(name)) {
            members.put(name, ref);
            return true;
        }
        return false;
    }

    private void removeMember(ActorRef<Msg> sender) {
        for(Map.Entry<String, ActorRef<Msg>> p : members.entrySet()) {
            if (p.getValue() == sender)
                members.remove(p.getKey());
        }
    }

    private void sendChat(Msg m) throws SuspendExecution {
        String senderName = "";
        ArrayList<ActorRef<Msg>> targets = new ArrayList<>();

        for(Map.Entry<String, ActorRef<Msg>> p : members.entrySet()) {
            if (p.getValue() != m.sender)
                targets.add(p.getValue());
            else
                senderName = p.getKey();

        }

        String[] attachment = new String[] { MessageBuilder.format(senderName, (String) m.content ) };

        targets.forEach(t -> {
            try {
                sendTo(t, Msg.Type.SENT_CHAT, attachment);
            } catch (SuspendExecution suspendExecution) {
                suspendExecution.printStackTrace();
            }
        });

        // notificationHandler.send( new Notification(Notification.Type.CHAT, senderName, name));
    }

    private String roomUsers() {
        // STOP. HAMMER TIME
        return members.keySet().toString() + "\n";
    }

    private void sendRoomUsers(ActorRef<Msg> target) throws SuspendExecution {
        sendTo(target, Msg.Type.ROOM_USERS, new String[] { roomUsers() });
    }


    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {
        while(
            receive(msg -> {
                ActorRef<Msg> sender = msg.sender;
                switch (msg.type) {
                    case JOIN:
                        addMember((String) msg.content, sender);
                        sendRoomUsers(sender);
                        notificationHandler.send( new Notification(Notification.Type.JOIN, (String) msg.content, name));
                        return true;
                    case LEAVE:
                        removeMember(sender);
                        // notificationHandler.send( new Notification(Notification.Type.LEAVE, (String) msg.content, name));
                        return true;
                    case SENT_CHAT:
                        sendChat(msg);
                        return true;
                    case GET_ROOM_USERS:
                        sendRoomUsers(sender);
                        return true;
                    case CLOSE:
                        kickMembers();
                        return false;
                }

                return false;
            }));
        return null;
    }
}

