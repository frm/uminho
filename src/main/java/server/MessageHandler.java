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
    private ActorRef notificationHandler;


    public MessageHandler(ActorRef<String> writer, ActorRef<Msg> userRepo, ActorRef<Msg> roomRepo, ActorRef<Notification> nh){
        currRoom = null;
        this.writer = writer;
        this.userRepo = userRepo;
        this.roomRepo = roomRepo;
        this.notificationHandler = nh;
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

    // CONNECT LOOP
    private void connectLoop() throws InterruptedException, SuspendExecution {
        while(!attemptConnection());
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
            default:
                return new Pair<>(false, MessageBuilder.INVALID_COMMAND);
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
            default:
                return new Pair<>(false, MessageBuilder.INVALID_COMMAND);
        }
    }

    // CONNECTION ACTIONS
    private Pair<Boolean, String> registerUser(String[] args) throws SuspendExecution, InterruptedException {
        sendTo(userRepo, Msg.Type.REGISTER, args);
        Boolean res = (Boolean)receive(msg -> msg.content);
        String reply;
        if(res) {
            // reply = TODO: list of channels here
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
        Boolean res = (Boolean)receive(msg -> msg.content);
        String reply;
        if(res) {
            // reply = TODO: list of channels here
            username = args[0];
            reply = MessageBuilder.message(MessageBuilder.AUTH_SUCCESS);
        }
        else {
            reply = MessageBuilder.message(MessageBuilder.AUTH_INVALID);
        }
        return new Pair<>(res, reply);
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

    private String validateRoom(Object o) {
        boolean b = o != null;

        if(b)
            currRoom = (ActorRef<Msg>)o;

        return MessageBuilder.message(b ? MessageBuilder.JOIN_SUCCESS : MessageBuilder.JOIN_INVALID);
    }

    private Pair<Boolean, String> sendPrivateMessage(String[] args) throws SuspendExecution, InterruptedException {
        // TODO: Implement this
        return new Pair<>(false, "LOL DIS DOESUN WORK");
    }

    private Pair<Boolean, String> listRooms() throws SuspendExecution, InterruptedException {
        sendTo(roomRepo, Msg.Type.GET_ROOMS, null);
        String list = (String)receive(msg -> msg.content);
        return new Pair<>(false, list);
    }

    // MAIN LOOP ACTIONS
    private void attemptJoin(String[] args) throws SuspendExecution, InterruptedException {
        sendTo(roomRepo, Msg.Type.JOIN, args);
    }

    private void messageRoom(String msg) throws SuspendExecution {
        sendTo(currRoom, Msg.Type.SENT_CHAT, msg);
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

    private void mainLoop() throws InterruptedException, SuspendExecution {
        while(
            receive(msg -> {
                ActorRef<Msg> sender = msg.sender;
                String[] args = (String[])msg.content;

                switch (msg.type) {
                    // FROM LINEREADER
                    case PM:
                        //TODO: Handle Private Messages (take into account //the receiver)
                        return true;
                    case CHAT:
                        messageRoom(args[0]);
                        return true;
                    case JOIN:
                        attemptJoin(new String[] { args[0], username });
                        return true;
                    case LEAVE:
                        leaveRoom();

                        try {
                            joinLoop();
                        } catch (IOException e) {
                            e.printStackTrace();
                        }

                        return true;
                    case GET_ROOM_USERS:
                        listUsers();
                        return true;
                    case GET_ROOMS:
                        getRooms();
                        return true;

                    //FROM ROOM REPO
                    case ROOM:
                        write( validateRoom(msg.content) );
                        return true;
                    case ROOMS:
                        write(args[0]);
                        // notificationHandler.send( new Notification//(Notification.Type.ROOM_LIST_REQUEST, null, self()));
                        return true;
                    //FROM ROOM
                    case SENT_CHAT:
                    case ROOM_USERS:
                        write(args[0]);
                        return true;
                    case KICK:
                        currRoom = null;
                        write(args[0]);
                        return true;
                    case PORT_LIST:
                        //writer.send((String) msg.content);
                        return true;
                }

                return false;
            }));
    }

    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {
        try {
            connectLoop();
            joinLoop();
            mainLoop();
        } catch (IOException e) {
            e.printStackTrace();
        }

        return null;
    }
}
