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
    //TODO: Change UserRepo to Actor
    private final UserRepo repo;
    private ActorRef roomRepo;
    private String currentUser;
    private ActorRef handler;

    public static final int DEFAULT_SIZE = 1024;

    public LineReader(FiberSocketChannel client, UserRepo users, ActorRef roomRepo) {
        cl = client;
        repo = users;
        buf = ByteBuffer.allocate(DEFAULT_SIZE);
        this.roomRepo = roomRepo;
    }

    public LineReader(FiberSocketChannel client, UserRepo users, int bufferCapacity) {
        cl = client;
        repo = users;
        buf = ByteBuffer.allocate(bufferCapacity);
    }

    private Pair<Boolean, String> registerUser(String uname, String password) {
        Boolean res = repo.register(uname, password, self());
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

    private Pair<Boolean, String> authenticateUser(String uname, String password) {
        Boolean res = repo.logIn(uname, password, self());
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

    private Pair<Boolean, String> deleteUser(String uname, String password) {
        Boolean res = repo.delete(uname, password);
        String reply = MessageBuilder.message(res ? MessageBuilder.DELETE_SUCCESS : MessageBuilder.INVALID_PARAMS);
        return new Pair<>(res, reply);
    }

    private Pair<Boolean, String> onConnection(String req) {
        Command c = Command.parse(req);
        switch(c.command) {
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

    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {
        boolean connected = false;
        boolean inRoom = false;

        try {
            while(!connected) {
                String req = read();
                Pair<Boolean, String> p = onConnection(req);
                connected = p.first;
                write(p.second);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        handler = (new MessageHandler(self(), roomRepo, currentUser)).spawn();


        System.out.println("BANJO");

        handler.send( new Msg(Msg.Type.JOIN, "room", self()) );

        //TODO: Use inRoom

        while (
                receive(msg -> {
                    ActorRef sender = msg.sender;

                    try {
                        switch (msg.type) {
                            case ROOMS:
                                write( msg.content.toString());
                                return true;
                            //FROM ROOM
                            case NEW_CHAT:
                                write( msg.content.toString());
                                return true;
                            case ROOM_USERS:
                                write( msg.content.toString());
                                return true;
                            case KICK:
                                write( "The room was closed, or you were kicked from it");
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


        if(connected)
            repo.disconnect(currentUser, self());

        return null;
    }
}
