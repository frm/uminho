package notification;

import co.paralleluniverse.actors.ActorRef;

import java.util.HashMap;

/**
 * Created by joaorodrigues on 5 Dec 15.
 */
public class Notification {
    public enum Type {JOIN, LEAVE, LOGIN, LOGOUT, SIGNUP, CREATE, REMOVE, CHAT, ROOM_LIST_REQUEST}

    public Type type;
    public String field1;
    public String field2;
    public ActorRef sender;

    private static final HashMap<Type ,String> builders = new HashMap<Type, String>() {{
        put(Type.JOIN, " has joined ");
        put(Type.LEAVE, " has left ");
        put(Type.LOGIN, " has logged in");
        put(Type.LOGOUT, " has logged out");
        put(Type.SIGNUP, " has signed up");
        put(Type.CREATE, " room was created");
        put(Type.REMOVE, " room was removed");
        put(Type.CHAT, " has messaged the room ");
        put(Type.ROOM_LIST_REQUEST, " has requested the room list");


    }};

    public Notification( Type t, String f1, String f2, ActorRef sender ){
        type = t;
        field1 = f1;
        field2 = f2;
        this.sender = sender;
    }

    public Notification( Type t, String f1, ActorRef sender){
        type = t;
        field1 = f1;
        field2 = "";
        this.sender = sender;
    }

    public Notification( Type t, String f1, String f2 ){
        type = t;
        field1 = f1;
        field2 = f2;
    }

    public Notification( Type t, String f1){
        type = t;
        field1 = f1;
        field2 = "";
    }

    @Override
    public String toString() {
        return field1+builders.get(type)+field2;
    }
}
