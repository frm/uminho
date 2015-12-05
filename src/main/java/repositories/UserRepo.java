package repositories;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import communication.Msg;
import models.User;

import java.util.HashMap;

/**
 * Created by frm on 28/11/15.
 */
public class UserRepo extends BasicActor<Msg, Void> {
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

    public boolean makeAdmin(String target, String admin) {
        User a = users.get(admin);
        User t = users.get(target);
        return (
                a != null
                && t != null
                && a.isAdmin()
                && t.addAdminPrivileges()
                );
    }

    public boolean revokeAdmin(String target, String admin) {
        User a = users.get(admin);
        User t = users.get(target);
        return (
                a != null
                && t != null
                && a.isAdmin()
                && a.authorityOver(t)
                && t.revokeAdminPrivileges()
        );
    }

    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {
        while(
                receive(msg -> {
                    String[] args = (String[])msg.content;
                    Boolean ans = false;
                    ActorRef<Msg> sender = msg.sender;
                    switch(msg.type) {
                        case REGISTER:
                            ans = register(args[0], args[1], sender);
                            break;
                        case AUTH:
                            ans = logIn(args[0], args[1], sender);
                            break;
                        case CANCEL:
                            ans = delete(args[0], args[1]);
                            break;
                        case DEAUTH:
                            ans = disconnect(args[0], sender);
                            break;
                    }

                    sender.send(new Msg(Msg.Type.OK, ans, self()));
                    return true;
                })
        );

        return null;
    }
}
