package server;

import java.util.HashMap;
import java.util.HashSet;

/**
 * Created by frm on 28/11/15.
 */
public class UserRepo {
    private HashMap<String, User> users;

    public UserRepo() {
        this.users = new HashMap<>();
    }

    public boolean logIn(String uname, String password) {
        User u = users.get(uname);
        return u != null && u.login(password);
    }

    public boolean register(String uname, String password) {
        // TODO: before entering this method, add nr of arguments validation. We don't want passwords with spaces
        if(users.containsKey(uname))
            return false;

        User u = new User(uname, password);
        u.login(password);
        users.put(uname, u);

        return true;
    }
}
