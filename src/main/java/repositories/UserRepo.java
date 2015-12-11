package repositories;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import communication.Msg;
import models.User;
import notification.Notification;
import util.MessageBuilder;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by frm on 28/11/15.
 */
public class UserRepo extends BasicActor<Msg, Void> {
    private HashMap<String, User> users;
    private ActorRef<Notification> notificationHandler;

    public UserRepo(ActorRef<Notification> nh) {
        this.users = new HashMap<>();
        this.notificationHandler = nh;
    }

    private User findByAddress(ActorRef<Msg> address) {
        for(User u : users.values())
            if(u.hasAddress(address))
                return u;

        return null;
    }

    public Boolean[] logIn(String uname, String password, ActorRef address) {
        User u = users.get(uname);
        return new Boolean[] { u != null && u.login(password, address), u.isAdmin() };
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

    private void grant(String uname, ActorRef<Msg> sender) throws SuspendExecution {
        String reply;
        Msg.Type type;
        User u = findByAddress(sender);
        if(u != null && makeAdmin(uname, u.uname)) {
            reply = MessageBuilder.message(MessageBuilder.GRANT_SUCCESS);
            type = Msg.Type.OK;
            sendTo(u.getAddress(), Msg.Type.GRANTED, null);
        }
        else {
            reply = MessageBuilder.message(MessageBuilder.GRANT_INVALID);
            type = Msg.Type.ERROR;
        }
        sendTo(sender, type, new String[] { reply });
    }

    private void revoke(String uname, ActorRef<Msg> sender) throws SuspendExecution {
        String reply;
        Msg.Type t;
        User usr = findByAddress(sender);
        if(usr != null && revokeAdmin(uname, usr.uname)) {
            reply = MessageBuilder.message(MessageBuilder.REVOKE_SUCCESS);
            t = Msg.Type.OK;
            sendTo(usr.getAddress(), Msg.Type.REVOKED, null);
        }
        else {
            reply = MessageBuilder.message(MessageBuilder.REVOKE_INVALID);
            t = Msg.Type.ERROR;
        }
        sendTo(sender, t, new String[] { reply });
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
                            if(ans)
                                notificationHandler.send( new Notification(Notification.Type.SIGNUP, args[0]));
                            sendTo(sender, Msg.Type.OK, ans);
                            break;
                        case AUTH:
                            Boolean[] b = logIn(args[0], args[1], sender);
                            if(b[0])
                                notificationHandler.send( new Notification(Notification.Type.LOGIN, args[0]));
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
                            notificationHandler.send( new Notification(Notification.Type.LOGOUT, args[0]));
                            break;
                        case PM:
                            pmUser(args[0], args[1], sender);
                            break;
                        case GRANT:
                            grant(args[0], sender);
                            break;
                        case REVOKE:
                            break;
                    }

                    return true;
                })
        );

        return null;
    }
}
