package repositories;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import communication.Msg;
import models.User;
import util.MessageBuilder;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by frm on 28/11/15.
 */
public class UserRepo extends BasicActor<Msg, Void> {
    private HashMap<String, User> users;

    public UserRepo() {
        this.users = new HashMap<>();
    }

    private User findByAddress(ActorRef<Msg> address) {
        for(User u : users.values())
            if(u.hasAddress(address))
                return u;

        return null;
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

    public boolean disconnect(ActorRef address) {
        User u = findByAddress(address);
        if (u != null)
            u.disconnect();
        return false;
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

    private void pmUser(String uname, String msg, ActorRef<Msg> sender) throws SuspendExecution {
        User u = users.get(uname);
        String senderName = findByAddress(sender).uname;
        if(u == null)
            sendTo(sender, Msg.Type.ERROR, new String[] { MessageBuilder.message(MessageBuilder.NO_SUCH_USER) });
        else
            sendTo(u.getAddress(), Msg.Type.SENT_PM, new String[]{MessageBuilder.formatPM(senderName, msg)});
    }

    private void sendTo(ActorRef<Msg> target, Msg.Type type, Object attachment) throws SuspendExecution {
        target.send(new Msg(type, attachment, self()));
    }

    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {
        while(
                receive(msg -> {
                    String[] args = (String[])msg.content;
                    ActorRef<Msg> sender = msg.sender;
                    switch(msg.type) {
                        case REGISTER:
                            boolean ans = register(args[0], args[1], sender);
                            sendTo(sender, Msg.Type.OK, ans);
                            break;
                        case AUTH:
                            Boolean b = logIn(args[0], args[1], sender);
                            sendTo(sender, Msg.Type.OK, b);
                            break;
                        case CANCEL:
                            Boolean c = delete(args[0], args[1]);
                            sendTo(sender, Msg.Type.OK, c);
                            break;
                        case DEAUTH:
                            disconnect(sender);
                            break;
                        case PM:
                            pmUser(args[0], args[1], sender);
                            break;
                    }

                    return true;
                })
        );

        return null;
    }
}
