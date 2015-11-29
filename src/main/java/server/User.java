package server;

import co.paralleluniverse.actors.ActorRef;

/**
 * Created by frm on 28/11/15.
 */
public class User {
    public String uname;
    private String password;
    private boolean logged; // TODO: can this be replaced by checking if the connection is null?
    private ActorRef connection;

    public User(String uname, String password) {
        this.uname = uname;
        this.password = password;
    }

    public boolean login(String password, ActorRef address) {
        if(logged)
            return false;

        boolean b = matchPassword(password);
        if(b) {
            logged = true;
            connection = address;
        }

        return b;
    }

    public boolean disconnect(ActorRef address) {
        boolean b = address.equals(connection);
        if(b) {
            logged = false;
            connection = null;
        }

        return b;
    }

    @Override
    public int hashCode() {
        return uname.hashCode();
    }

    public boolean matchPassword(String password) {
        return this.password.equals(password);
    }

    public boolean isLoggedIn() {
        return logged;
    }
}
