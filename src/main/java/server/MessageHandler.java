package server;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import co.paralleluniverse.fibers.Suspendable;
import communication.Msg;
import notification.Notification;
import util.MessageBuilder;
import util.Pair;

import java.io.IOException;

/**
 * Created by joaorodrigues on 2 Dec 15.
 */
public class MessageHandler extends BasicActor<Msg, Void> {
    private ActorRef<Msg> currRoom;
    private ActorRef<Msg> userRepo;
    private ActorRef<Msg> roomRepo;
    private ActorRef<String> writer;
    private String username;
    private boolean connected;
    private boolean admin;
    private ActorRef notificationHandler;


    public MessageHandler(ActorRef<String> writer, ActorRef<Msg> userRepo, ActorRef<Msg> roomRepo, ActorRef<Notification> nh){
        currRoom = null;
        this.writer = writer;
        this.userRepo = userRepo;
        this.roomRepo = roomRepo;
        this.notificationHandler = nh;
        connected = false;
        admin = false;
    }

    // INTERNAL API

    private void write(String s) throws SuspendExecution {
        if(s != null && s.length() > 0)
            writer.send(s);
    }

    private void sendTo(ActorRef target, Msg.Type type, Object args) throws SuspendExecution {
        target.send(new Msg(type, args, self()));
    }

    private boolean sendToUser(Pair<Boolean, String> p) throws SuspendExecution {
        write(p.second);
        return p.first;
    }

    private void disconnect() throws SuspendExecution {
        connected = false;
        writer.send(null);
        if(currRoom != null)
            leaveRoom();
        deauth();
    }

    // CONNECT LOOP
    private void connectLoop() throws InterruptedException, SuspendExecution {
        while(! (connected = attemptConnection()) );
    }

    private boolean attemptConnection() throws SuspendExecution, InterruptedException {
        return sendToUser( onConnection( receive() ) );
    }

    private Pair<Boolean, String> onConnection(Msg m) throws InterruptedException, SuspendExecution {
        String[] args = (String[])m.content;
        switch(m.type) {
            case REGISTER:
                return registerUser(args);
            case AUTH:
                return authenticateUser(args);
            case CANCEL:
                return deleteUser(args);
            case DEAUTH:
                disconnect();
                return new Pair<>(true, "");
            default:
                return new Pair<>(false, MessageBuilder.message(MessageBuilder.NOT_AUTHENTICATED));
        }
    }

    // JOIN LOOP
    private void joinLoop() throws IOException, SuspendExecution, InterruptedException {
        while( !attemptJoin() );
    }

    private boolean attemptJoin() throws IOException, SuspendExecution, InterruptedException {
        return sendToUser( joinOrMessage( receive() ) );
    }

    private Pair<Boolean, String> joinOrMessage(Msg m) throws InterruptedException, SuspendExecution {
        String[] args = (String[])m.content;
        switch(m.type) {
            case JOIN:
                return joinRoom(new String[]{ args[0], username });
            case PM:
                return sendPrivateMessage(args);
            case GET_ROOMS:
                return listRooms();
            case DEAUTH:
                disconnect();
                return new Pair<>(true, "");
            default:
                return new Pair<>(false, MessageBuilder.message(MessageBuilder.INVALID_COMMAND));
        }
    }

    // CONNECTION ACTIONS
    private Pair<Boolean, String> registerUser(String[] args) throws SuspendExecution, InterruptedException {
        sendTo(userRepo, Msg.Type.REGISTER, args);
        Boolean res = (Boolean)receive(msg -> msg.content);
        String reply;
        if(res) {
            username = args[0];
            reply = MessageBuilder.message(MessageBuilder.REGISTER_SUCCESS);
        }
        else {
            reply = MessageBuilder.message(MessageBuilder.REGISTER_INVALID);
        }
        return new Pair<>(res, reply);
    }

    private Pair<Boolean, String> authenticateUser(String[] args) throws InterruptedException, SuspendExecution {
        sendTo(userRepo, Msg.Type.AUTH, args);
        Boolean[] res = (Boolean[])receive(msg -> msg.content);
        String reply;
        if(res[0]) {
            username = args[0];
            admin = res[1];
            reply = MessageBuilder.message(MessageBuilder.AUTH_SUCCESS);
        }
        else {
            reply = MessageBuilder.message(MessageBuilder.AUTH_INVALID);
        }
        return new Pair<>(res[0], reply);
    }

    private Pair<Boolean, String> deleteUser(String[] args) throws InterruptedException, SuspendExecution {
        sendTo(userRepo, Msg.Type.CANCEL, args);
        Boolean res = (Boolean)receive(msg -> msg.content);
        String reply = MessageBuilder.message(res ? MessageBuilder.DELETE_SUCCESS : MessageBuilder.INVALID_PARAMS);
        return new Pair<>(res, reply);
    }

    // JOIN LOOP ACTIONS
    @Suspendable
    private Pair<Boolean, String> joinRoom(String[] args) throws SuspendExecution, InterruptedException {
        sendTo(roomRepo, Msg.Type.JOIN, args);
        Msg m = receive();
        boolean res = m.content != null;
        if(res)
            currRoom = (ActorRef<Msg>)m.content;
        String reply = MessageBuilder.message(res ? MessageBuilder.JOIN_SUCCESS : MessageBuilder.JOIN_INVALID);
        return new Pair<>(res, reply);
    }

    private String validateRoom(Object o) throws SuspendExecution {
        boolean b = o != null;

        if(b) {
            if(currRoom != null)
                leaveRoom();
            currRoom = (ActorRef<Msg>) o;
        }

        return MessageBuilder.message(b ? MessageBuilder.JOIN_SUCCESS : MessageBuilder.JOIN_INVALID);
    }

    private Pair<Boolean, String> sendPrivateMessage(String[] args) throws SuspendExecution, InterruptedException {
        // TODO: Implement this
        return new Pair<>(false, "LOL DIS DOESUN WORK");
    }

    private Pair<Boolean, String> listRooms() throws SuspendExecution, InterruptedException {
        sendTo(roomRepo, Msg.Type.GET_ROOMS, null);
        String list = (String)receive(msg -> ((String[])msg.content)[0]);
        return new Pair<>(false, list);
    }

    // MAIN LOOP ACTIONS
    private void attemptJoin(String[] args) throws SuspendExecution, InterruptedException {
        sendTo(roomRepo, Msg.Type.JOIN, args);
    }

    private void messageRoom(String msg) throws SuspendExecution {
        sendTo(currRoom, Msg.Type.SENT_CHAT, msg);
    }

    private void pmUser(String[] args) throws SuspendExecution {
        sendTo(userRepo, Msg.Type.PM, args);
    }

    private void leaveRoom() throws SuspendExecution {
        sendTo(currRoom, Msg.Type.LEAVE, null);
        currRoom = null;
    }

    private void listUsers() throws SuspendExecution {
        sendTo(currRoom, Msg.Type.GET_ROOM_USERS, null);
    }

    private void getRooms() throws SuspendExecution {
        sendTo(roomRepo, Msg.Type.GET_ROOMS, null);
    }

    private void deauth() throws SuspendExecution {
        sendTo(userRepo, Msg.Type.DEAUTH, null);
    }

    private void cancelAccount(String[] args) throws SuspendExecution {
        sendTo(userRepo, Msg.Type.CANCEL, args);
    }

    private void grant(String[] args) throws SuspendExecution {
        sendTo(userRepo, Msg.Type.GRANT, args);
    }

    private void revoke(String[] args) throws SuspendExecution {
        sendTo(userRepo, Msg.Type.REVOKE, args);
    }

    private void getGranted() throws SuspendExecution {
        admin = true;
        write(MessageBuilder.message(MessageBuilder.GRANTED));
    }

    private void getRevoked() throws SuspendExecution {
        admin = false;
        write(MessageBuilder.message(MessageBuilder.REVOKED));
    }

    private void addRoom(String[] args) throws SuspendExecution {
        sendTo(roomRepo, Msg.Type.ADD, args);
    }

    private void removeRoom(String[] args) throws SuspendExecution {
        sendTo(roomRepo, Msg.Type.REMOVE, args);
    }

    private void mainLoop() throws InterruptedException, SuspendExecution {
        // if disconnected on the join loop
        if(!connected)
            return;

        while(
            receive(msg -> {
                ActorRef<Msg> sender = msg.sender;

                if(msg.type == Msg.Type.ROOM) {
                    write( validateRoom(msg.content) );
                    return true;
                }

                String[] args = (String[])msg.content;

                switch (msg.type) {
                    // FROM LINEREADER
                    case PM:
                        pmUser(args);
                        return true;
                    case CHAT:
                        messageRoom(args[0]);
                        return true;
                    case JOIN:
                        attemptJoin(new String[] { args[0], username });
                        return true;
                    case LEAVE:
                        leaveRoom();
                        return false;
                    case GET_ROOM_USERS:
                        listUsers();
                        return true;
                    case GET_ROOMS:
                        getRooms();
                        return true;
                    case CANCEL:
                        cancelAccount(args);
                        return true;
                    case GRANT:
                        grant(args);
                        return true;
                    case REVOKE:
                        revoke(args);
                        return true;
                    case ADD:
                        if(admin)
                            addRoom(args);
                        else
                            write(MessageBuilder.message(MessageBuilder.INVALID_COMMAND));
                        return true;
                    case REMOVE:
                        if(admin)
                            removeRoom(args);
                        else
                            write(MessageBuilder.message(MessageBuilder.INVALID_COMMAND));
                        return true;

                    //FROM ROOM REPO
                    case ROOMS:
                        write(args[0]);
                        // notificationHandler.send( new Notification//(Notification.Type.ROOM_LIST_REQUEST, null, self()));
                        return true;
                    //FROM ROOM
                    case SENT_PM:
                    case SENT_CHAT:
                    case ROOM_USERS:
                        write(args[0]);
                        return true;
                    case KICK:
                        currRoom = null;
                        write(MessageBuilder.message(MessageBuilder.KICKED));
                        return true;
                    case JOINED_ROOM:
                        write(MessageBuilder.formatRoomActivity(args[0], MessageBuilder.JOINED_ROOM));
                        return true;
                    case LEFT_ROOM:
                        write(MessageBuilder.formatRoomActivity(args[0], MessageBuilder.LEFT_ROOM));
                        return true;
                    // FROM USER REPO
                    case GRANTED:
                        getGranted();
                        return true;
                    case REVOKED:
                        getRevoked();
                        return true;
                    // GENERAL
                    case PORT_LIST:
                        //writer.send((String) msg.content);
                        return true;
                    case DEAUTH:
                        disconnect();
                        return false;
                    case OK:
                    case ERROR:
                        write(args[0]);
                        return true;
                }

                return false;
            }));
    }

    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {
        try {
            connectLoop();
            while(connected) {
                joinLoop();
                mainLoop();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        return null;
    }
}
