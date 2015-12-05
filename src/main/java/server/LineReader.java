package server;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import co.paralleluniverse.fibers.Suspendable;
import co.paralleluniverse.fibers.io.FiberSocketChannel;
import communication.Command;
import repositories.UserRepo;
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
    private ActorRef userRepo;
    private ActorRef roomRepo;
    private String currentUser;
    private ActorRef handler;
    private ActorRef notificationHandler;

    public static final int DEFAULT_SIZE = 1024;

    public LineReader(FiberSocketChannel client, ActorRef users, ActorRef roomRepo, ActorRef nh) {
        cl = client;
        userRepo = users;
        buf = ByteBuffer.allocate(DEFAULT_SIZE);
        this.roomRepo = roomRepo;
        notificationHandler = nh;
    }

    private Pair<Boolean, String> registerUser(String uname, String password) throws InterruptedException, SuspendExecution {
        userRepo.send( new Msg(Msg.Type.REGISTER, new String[] { uname, password }, self()) );
        Boolean res = (Boolean)receive(msg -> msg.content);
        String reply = null;
        if(res) {
            // reply = TODO: list of channels here
            currentUser = uname;
            reply = MessageBuilder.message(MessageBuilder.REGISTER_SUCCESS);
        }
        else {
            reply = MessageBuilder.message(MessageBuilder.REGISTER_INVALID);
        }
        return new Pair<>(res, reply);
    }

    private Pair<Boolean, String> authenticateUser(String uname, String password) throws InterruptedException, SuspendExecution {
        userRepo.send( new Msg(Msg.Type.AUTH, new String[] { uname, password }, self()) );
        Boolean res = (Boolean)receive(msg -> msg.content);
        String reply = null;
        if(res) {
            // reply = TODO: list of channels here
            currentUser = uname;
            reply = MessageBuilder.message(MessageBuilder.AUTH_SUCCESS);
        }
        else {
            reply = MessageBuilder.message(MessageBuilder.AUTH_INVALID);
        }
        return new Pair<>(res, reply);
    }

    private Pair<Boolean, String> deleteUser(String uname, String password) throws InterruptedException, SuspendExecution {
        userRepo.send( new Msg(Msg.Type.CANCEL, new String[]{ uname, password }, self()) );
        Boolean res = (Boolean)receive(msg -> msg.content);
        String reply = MessageBuilder.message(res ? MessageBuilder.DELETE_SUCCESS : MessageBuilder.INVALID_PARAMS);
        return new Pair<>(res, reply);
    }

    private Pair<Boolean, String> onConnection(String req) throws SuspendExecution, InterruptedException {
        Command c = Command.parse(req);
        switch(c.command) {
            // @jorod: ahead I am switching back to string array to send. Should I change the method signature?
            case Command.REGISTER:
                return registerUser(c.args[0], c.args[1]);
            case Command.AUTHENTICATE:
                return authenticateUser(c.args[0], c.args[1]);
            case Command.CANCEL:
                return deleteUser(c.args[0], c.args[1]);
            default:
                return new Pair<>(false, MessageBuilder.message(MessageBuilder.INVALID_COMMAND));
        }
    }

    @Suspendable
    private String read() throws IOException {
        buf.clear();
        cl.read(buf);
        buf.flip();
        return new String(buf.array(), 0, buf.limit());
    }

    @Suspendable
    private void write(String s) throws IOException {
        buf.clear();
        buf.put(s.getBytes());
        buf.flip();
        cl.write(buf);
    }

    private boolean attemptConnection() throws IOException, InterruptedException, SuspendExecution {
        String req = read();
        Pair<Boolean, String> p = onConnection(req);
        boolean connected = p.first;
        write(p.second);
        return connected;
    }

    private void disconnect() throws SuspendExecution {
        userRepo.send(new Msg(Msg.Type.DEAUTH, new String[] { currentUser }, self()));
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
                                write( msg.content.toString());
                                return true;
                            case SENT_CHAT:
                                write( msg.content.toString());
                                return true;
                            case ROOM_USERS:
                                write( msg.content.toString());
                                return true;
                            case KICK:
                                write( "The room was closed, or you were kicked from it"); // @jorod: this should use the config file
                                return true;
                            case OK:
                                write( msg.content.toString());
                                return true;
                        }
                    } catch (IOException e) {
                        e.printStackTrace();
                    }

                    return false;
                })) ;
    }

    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {
        boolean connected = false;

        try {
            while(! (connected = attemptConnection()) );
        } catch (IOException e) {
            e.printStackTrace();
        }

        initHandler();

        handler.send(new Msg(Msg.Type.JOIN, "banjo", self()));

        receiveLoop();

        if(connected)
            disconnect();

        return null;
    }
}
