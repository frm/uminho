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

    // TODO: change this method to also save the actor ref
    public boolean login(String password) {
        if(logged)
            return false;

        boolean b = this.password.equals(password);
        if(b) {
            logged = true;
            // save actor ref here
        }

        return b;
    }

    @Override
    public int hashCode() {
        return uname.hashCode();
    }
}
