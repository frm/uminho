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

    private void closeRoom() throws SuspendExecution {
        for (ActorRef usr : members.values()) {
            usr.send(new Msg(Msg.Type.REMOVE, null, self()));
        }
    }

    private boolean addMember(String name, ActorRef ref) {
        if (!members.containsKey(name)) {
            members.put(name, ref);
            return true;
        }
        return false;
    }

    private boolean removeMember(String name, ActorRef ref) {
        ActorRef usr = members.get(name);
        if (usr == null || !usr.equals(ref))
            return false;
        members.remove(name);
        return true;

    }

    private void sendChat(Msg message) throws SuspendExecution {
        for (ActorRef ref : members.values()) {
            if (ref != message.sender)
                ref.send(message);
        }
    }


    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {
        receive(msg -> {
            ActorRef sender = msg.sender;
            switch (msg.type) {
                case ADD:
                    if (addMember((String) msg.content, sender))
                        sender.send(new Msg(Msg.Type.OK, null, self()));
                    else
                        sender.send(new Msg(Msg.Type.ERROR, null, self()));
                    return true;
                case CHAT:
                    sendChat(msg);
                    return true;
                case REMOVE:
                    if (removeMember((String) msg.content, sender))
                        sender.send(new Msg(Msg.Type.OK, null, self()));
                    else
                        sender.send(new Msg(Msg.Type.ERROR, null, self()));
                    return true;
            }

            return false;
        });
        return null;
    }
}

