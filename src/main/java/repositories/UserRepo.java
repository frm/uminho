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

    public boolean delete(String uname, String password, ActorRef<Msg> sender) {
        User u = users.get(uname);
        boolean b = u != null && u.matchPassword(password) && u.hasAddress(sender);
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
        User defaultAdmin = new User("admin", "admin");
        defaultAdmin.addAdminPrivileges();
        users.put("admin", defaultAdmin);

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
                            Msg.Type t;
                            Object o = null;
                            if(delete(args[0], args[1], sender)) {
                                t = Msg.Type.DEAUTH;
                            }
                            else {
                                t = Msg.Type.ERROR;
                                o = new String[]{MessageBuilder.message(MessageBuilder.AUTH_INVALID)};
                            }

                            sendTo(sender, t, o);
                            break;
                        case DEAUTH:
                            disconnect(sender);
                            break;
                        case PM:
                            pmUser(args[0], args[1], sender);
                            break;
                        case GRANT:
                            String reply;
                            Msg.Type type;
                            User u = findByAddress(sender);
                            if(u != null && makeAdmin(args[0], u.uname)) {
                                reply = MessageBuilder.message(MessageBuilder.GRANT_SUCCESS);
                                type = Msg.Type.OK;
                            }
                            else {
                                reply = MessageBuilder.message(MessageBuilder.GRANT_INVALID);
                                type = Msg.Type.ERROR;
                            }
                            sendTo(sender, type, new String[] { reply });
                            break;
                        case REVOKE:
                            String str;
                            Msg.Type rt;
                            User usr = findByAddress(sender);
                            if(usr != null && revokeAdmin(args[0], usr.uname)) {
                                str = MessageBuilder.message(MessageBuilder.REVOKE_SUCCESS);
                                rt = Msg.Type.OK;
                            }
                            else {
                                str = MessageBuilder.message(MessageBuilder.REVOKE_INVALID);
                                rt = Msg.Type.ERROR;
                            }
                            sendTo(sender, rt, new String[] { str });
                            break;
                    }

                    return true;
                })
        );

        return null;
    }
}
