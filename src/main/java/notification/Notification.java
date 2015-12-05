package notification;

import java.util.HashMap;

/**
 * Created by joaorodrigues on 5 Dec 15.
 */
public class Notification {
    public enum Type {JOIN, LEAVE, LOGIN, LOGOUT, SIGNUP, CREATE, REMOVE}

    public Type type;
    public String field1;
    public String field2;

    private static final HashMap<Type ,String> builders = new HashMap<Type, String>() {{
        put(Type.JOIN, " has joined ");
        put(Type.LEAVE, " has left ");
        put(Type.LOGIN, " has logged in.");
        put(Type.LOGOUT, " has logged out.");
        put(Type.SIGNUP, " has signed up.");
        put(Type.CREATE, " room was created.");
        put(Type.REMOVE, " room was removed.");

    }};

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
