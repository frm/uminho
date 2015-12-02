package server;

import co.paralleluniverse.actors.ActorRef;

import java.util.HashMap;
import java.util.HashSet;

/**
 * Created by frm on 28/11/15.
 */
public class UserRepo{
    private HashMap<String, User> users;

    public UserRepo() {
        this.users = new HashMap<>();
    }

    public boolean logIn(String uname, String password, ActorRef address) {
        User u = users.get(uname);
        return u != null && u.login(password, address);
    }

    public boolean register(String uname, String password, ActorRef address) {
        if(users.containsKey(uname))
            return false;

        User u = new User(uname, password);
        u.login(password, address);
        users.put(uname, u);

        return true;
    }

    public boolean delete(String uname, String password) {
        User u = users.get(uname);
        boolean b = u != null && u.matchPassword(password) && !u.isLoggedIn();
        if(b)
            users.remove(uname);

        return b;
    }

    public boolean disconnect(String uname, ActorRef address) {
        User u = users.get(uname);
        return u != null && u.disconnect(address);
    }
}
