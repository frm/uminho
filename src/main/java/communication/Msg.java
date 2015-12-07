package communication;

import co.paralleluniverse.actors.ActorRef;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by joaorodrigues on 1 Dec 15.
 */



public class Msg {
    public enum Type {OK, ERROR, CHAT, ADD, REMOVE, PM, GET_ROOM, ROOM, NEW_CHAT,
        GET_ROOM_USERS,ROOM_USERS, JOIN, LEAVE, GET_ROOMS, ROOMS, KICK, CLOSE,
        CANCEL, AUTH, REGISTER, DEAUTH,
        PORT_LIST}

    public final Type type;
    public final Object content;
    public final ActorRef sender;

    private static final Map<String, Type> commandInterface =
            new HashMap<String, Type>() {{
                put(Command.REGISTER, Type.REGISTER);
                put(Command.AUTHENTICATE, Type.AUTH);
                put(Command.CANCEL, Type.CANCEL);
                put(Command.JOIN, Type.JOIN);
                put(Command.LIST_ROOMS, Type.GET_ROOMS);
                put(Command.PM, Type.PM);
    }};

    public Msg(Type type, Object content, ActorRef sender){
      this.type = type;
      this.content = content;
      this.sender = sender;
    }

    public static Type commandType(String command) {
        return commandInterface.get(command);
    }
}
