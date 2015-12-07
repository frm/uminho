package server;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
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


    public MessageHandler(String username, ActorRef<String> writer, ActorRef<Msg> userRepo, ActorRef<Msg> roomRepo, ActorRef<Notification> nh){
        currRoom = null;
        this.writer = writer;
        this.userRepo = userRepo;
        this.roomRepo = roomRepo;
        this.username = username;
        this.notificationHandler = nh;
    }

    // INTERNAL API

    private void write(String s) throws SuspendExecution {
        if(s != null && s.length() > 0)
            writer.send(s);
    }

    private void sendTo(ActorRef target, Msg.Type type, String[] args) throws SuspendExecution {
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
        return sendToUser( onConnection( read() ) );
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
                return joinRoom(args);
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
    private Pair<Boolean, String> joinRoom(String[] args) throws SuspendExecution, InterruptedException {
        sendTo(roomRepo, Msg.Type.JOIN, args);
        Boolean res = (Boolean)receive(msg -> msg.content);
        String reply = MessageBuilder.message(res ? MessageBuilder.JOIN_SUCCESS : MessageBuilder.JOIN_INVALID);
        return new Pair<>(res, reply);
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

    private void mainLoop() {

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
//    @Override
//    protected Void doRun() throws InterruptedException, SuspendExecution {
//        while(
//            receive(msg -> {
//                ActorRef sender = msg.sender;
//                switch (msg.type) {
//                    //FROM LINEREADER
//                    case PM:
//                        //TODO: Handle Private Messages (take into account //the receiver)
//                        return true;
//                    case CHAT:
//                        currRoom.send( new Msg(Msg.Type.SENT_CHAT, //username+": "+ msg.content, self()));
//                        return true;
//                    case JOIN:
//                        roomRepo.send( new Msg(Msg.Type.GET_ROOM, (String) //msg.content, self()));
//                        return true;
//                    case LEAVE:
//                        currRoom.send( new Msg(Msg.Type.REMOVE,username, //self()));
//                        return true;
//                    case GET_ROOM_USERS:
//                        currRoom.send( new Msg(Msg.Type.GET_ROOM_USERS, //null, self()));
//                        notificationHandler.send( new Notification//(Notification.Type.ROOM_LIST_REQUEST, null, self()));
//                        return true;
//                    case GET_ROOMS:
//                        roomRepo.send( new Msg(Msg.Type.GET_ROOMS, null, //self()));
//                    //FROM ROOM REPO
//                    case ROOM:
//                        currRoom = (ActorRef) msg.content;
//                        joinRoom();
//                        return true;
//                    case ROOMS:
//                        user.send(msg);
//                        return true;
//                    //FROM ROOM
//                    case SENT_CHAT:
//                        user.send(msg);
//                        return true;
//                    case NEW_CHAT:
//                    case ROOM_USERS:
//                    case OK:
//                        user.send(msg);
//                        return true;
//                    case KICK:
//                        currRoom = null;
//                        user.send(msg);
//                        return true;
//                    case PORT_LIST:
//                        //writer.send((String) msg.content);
//                }
//
//                return false;
//            }));
//
//        return null;
//    }
}
