package server;

import co.paralleluniverse.actors.Actor;
import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import util.Msg;

import java.util.HashMap;



/**
 * Created by joaorodrigues on 30 Nov 15.
 */
public class Room extends BasicActor<Msg, Void> {
    private HashMap<String, ActorRef> members;

    public Room() {
        members = new HashMap<String, ActorRef>();

    }

    private void kickMembers() throws SuspendExecution {
        for (ActorRef member : members.values()) {
            member.send(new Msg(Msg.Type.KICK, null, self()));
        }
    }

    private boolean addMember(String name, ActorRef ref) {
        if (!members.containsKey(name)) {
            members.put(name, ref);
            return true;
        }
        return false;
    }

    private void removeMember(String name) {
        members.remove(name);

    }

    private void sendChat(Msg message) throws SuspendExecution {
        for (ActorRef ref : members.values()) {
            if (ref != message.sender)
                ref.send(message);
        }
    }


    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {
        while(
            receive(msg -> {
                ActorRef sender = msg.sender;
                switch (msg.type) {
                    case ADD:
                        addMember((String) msg.content, sender);
                        sender.send(new Msg(Msg.Type.ROOM_USERS, members.keySet(), self()));
                        return true;
                    case CHAT:
                        sendChat(new Msg(Msg.Type.NEW_CHAT, msg.content, sender));
                        return true;
                    case REMOVE:
                        removeMember((String) msg.content);
                        sender.send( new Msg(Msg.Type.OK, "Left Room, users are now: "+members.keySet(), self()));
                        return true;
                    case GET_ROOM_USERS:
                        sender.send(new Msg(Msg.Type.ROOM_USERS, members.keySet(), self()));
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

