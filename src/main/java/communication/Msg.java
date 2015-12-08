package communication;

import co.paralleluniverse.actors.ActorRef;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by joaorodrigues on 1 Dec 15.
 */



public class Msg {
    public enum Type {OK, ERROR, CHAT, ADD, REMOVE, PM, GET_ROOM, ROOM,
        GET_ROOM_USERS,ROOM_USERS, JOIN, LEAVE, GET_ROOMS, ROOMS, KICK, CLOSE,
        SENT_PM, SENT_CHAT,
        CANCEL, AUTH, REGISTER, DEAUTH,
        PORT_LIST}

    public final Type type;
    public final Object content;
    public final ActorRef sender;

    private static final Map<String, Type> commandInterface =
            new HashMap<String, Type>() {{
                put(Command.REGISTER, Type.REGISTER);
                put(Command.AUTHENTICATE, Type.AUTH);
                put(Command.DEAUTHENTICATE, Type.DEAUTH);
                put(Command.CANCEL, Type.CANCEL);
                put(Command.JOIN, Type.JOIN);
                put(Command.LIST_ROOMS, Type.GET_ROOMS);
                put(Command.LIST_ROOM_USERS, Type.GET_ROOM_USERS);
                put(Command.PM, Type.PM);
                put(Command.LEAVE, Type.LEAVE);
    }};

    public Msg(Type type, Object content, ActorRef sender){
      this.type = type;
      this.content = content;
      this.sender = sender;
    }

    public static Type commandType(String command) {
        Type t = commandInterface.get(command);
        return t != null ? t : Msg.Type.CHAT;
    }
}
