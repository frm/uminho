package server;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import co.paralleluniverse.fibers.Suspendable;
import co.paralleluniverse.fibers.io.FiberSocketChannel;
import communication.Command;
import util.MessageBuilder;
import communication.Msg;
import util.Pair;

import java.io.IOException;
import java.nio.ByteBuffer;

/**
 * Created by frm on 28/11/15.
 */
public class LineReader extends BasicActor<Msg, Void> {
    private final FiberSocketChannel cl;
    private final ByteBuffer buf;
    // TODO: Change this ActorRef to ActorRef<Msg, Void>
    private ActorRef userRepo;
    private ActorRef roomRepo;
    private ActorRef<String> writer;
    private ActorRef handler;
    private ActorRef notificationHandler;
    private String currentUser;
    private ActorRef replyTarget;

    public static final int DEFAULT_SIZE = 1024;

    public LineReader(FiberSocketChannel client, ActorRef users, ActorRef roomRepo, ActorRef nh) {
        cl = client;
        userRepo = users;
        buf = ByteBuffer.allocate(DEFAULT_SIZE);
        this.roomRepo = roomRepo;
        notificationHandler = nh;
    }

    // INTERNAL API
    private void init() {
        writer = (new LineWriter(cl)).spawn();
        handler = (new MessageHandler(self(), writer, roomRepo, currentUser)).spawn();
        replyTarget = self();
    }

    private void disconnect() throws SuspendExecution {
        // TODO: Gracefully disconnect here
        userRepo.send(new Msg(Msg.Type.DEAUTH, new String[] { currentUser }, self()));
    }

    @Suspendable
    private String read() throws IOException {
        buf.clear();
        cl.read(buf);
        buf.flip();
        return new String(buf.array(), 0, buf.limit());
    }

    private void write(String s) throws IOException, SuspendExecution {
        if(s != null && s.length() > 0)
            writer.send(s);
    }

    // TODO: Change this ActorRef to ActorRef<Msg, Void>
    private void sendTo(ActorRef target, Msg.Type type, Object attachment) throws SuspendExecution {
        target.send(new Msg(type, attachment, replyTarget));
    }

    private void replyToUser(String s) throws IOException, SuspendExecution {
        write(s);
    }

    private boolean replyToUser(Pair<Boolean, String> p) throws IOException, SuspendExecution {
        write(p.second);
        return p.first;
    }

    // CONNECTION LOOP
    private void connectLoop() throws InterruptedException, SuspendExecution, IOException {
        while(!attemptConnection());
    }

    private boolean attemptConnection() throws IOException, InterruptedException, SuspendExecution {
        return replyToUser( onConnection( read() ) );
    }

    private Pair<Boolean, String> onConnection(String req) throws SuspendExecution, InterruptedException {
        Command c = Command.parse(req);
        switch(c.command) {
            case Command.REGISTER:
                return registerUser(c.args);
            case Command.AUTHENTICATE:
                return authenticateUser(c.args);
            case Command.CANCEL:
                return deleteUser(c.args);
            default:
                return new Pair<>(false, MessageBuilder.message(MessageBuilder.INVALID_COMMAND));
        }
    }

    // JOIN LOOP
    private void joinLoop() throws IOException, SuspendExecution, InterruptedException {
        while( !attemptJoin() );
    }

    private boolean attemptJoin() throws IOException, SuspendExecution, InterruptedException {
        return replyToUser( joinOrMessage( read() ) );
    }

    private Pair<Boolean, String> joinOrMessage(String req) throws InterruptedException, SuspendExecution {
        Command c = Command.parse(req);
        switch(c.command) {
            case Command.JOIN:
                return joinRoom(c.args);
            case Command.PM:
                return sendPrivateMessage(c.args);
            case Command.LIST_ROOMS:
                return listRooms();
            default:
                return new Pair<>(false, MessageBuilder.message(MessageBuilder.INVALID_COMMAND));
        }
    }

    // READ LOOP
    private void readLoop() {
        replyTarget = writer;

    }

    // ON CONNECTION ACTIONS
    private Pair<Boolean, String> registerUser(String[] args) throws InterruptedException, SuspendExecution {
        sendTo(userRepo, Msg.Type.REGISTER, args);
        Boolean res = (Boolean)receive(msg -> msg.content);
        String reply;
        if(res) {
            // reply = TODO: list of channels here
            currentUser = args[0];
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
            currentUser = args[0];
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
        sendTo(roomRepo, Msg.Type.JOIN, args[0]);
        Boolean res = (Boolean)receive(msg -> msg.content);
        String reply = MessageBuilder.message(res ? MessageBuilder.JOIN_SUCCESS : MessageBuilder.JOIN_INVALID);
        return new Pair<>(res, reply);
    }

    private Pair<Boolean, String> sendPrivateMessage(String[] args) throws SuspendExecution, InterruptedException {
        // TODO: Implement this
        return new Pair<>(false, "LOL DIS DOESUN WORK");
    }

    private void initHandler() {
        handler = (new MessageHandler(self(), roomRepo, currentUser, notificationHandler)).spawn();
    }

    private void receiveLoop() throws InterruptedException, SuspendExecution {
        boolean inRoom = false;
        //TODO: Use inRoom

        while (
                receive(msg -> {
                    ActorRef sender = msg.sender; // @jorod: I don't think this is needed
                    System.out.println(msg.content);

                    try {
                        switch (msg.type) {
                            case ROOMS:
                                write(msg.content.toString());
                                return true;
                            case SENT_CHAT:
                                write(msg.content.toString());
                                return true;
                            case ROOM_USERS:
                                write(msg.content.toString());
                                return true;
                            case KICK:
                                write("The room was closed, or you were kicked from it"); // @jorod: this should use the config file
                                return true;
                            case OK:
                                write(msg.content.toString());
                                return true;
                        }
                    } catch (IOException e) {
                        e.printStackTrace();
                    }

                    return false;
                })) ;
    }

    private Pair<Boolean, String> listRooms() throws SuspendExecution, InterruptedException {
        sendTo(roomRepo, Msg.Type.GET_ROOMS, null);
        String list = (String)receive(msg -> msg.content);
        return new Pair<>(true, list);
    }

    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {
        // This might seem odd
        // but we only run the disconnect method if we have connected
        // if an exception occurred, connected is still false because it was stuck in connectLoop
        // otherwise, it completed the connectLoop and we will have to disconnect it
        boolean connected = false;

        try {
            init();
            connectLoop();
            connected = true;
            joinLoop();
            readLoop();

        } catch (IOException e) {
            e.printStackTrace();
        }

        if(connected)
            disconnect();

        return null;
    }
}
