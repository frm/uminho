package server;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import co.paralleluniverse.fibers.Suspendable;
import co.paralleluniverse.fibers.io.FiberSocketChannel;
import notification.Notification;
import communication.Msg;

import java.io.IOException;
import java.nio.ByteBuffer;

/**
 * Created by frm on 28/11/15.
 */
public class LineReader extends BasicActor<Msg, Void> {
    private final FiberSocketChannel cl;
    private final ByteBuffer buf;
    // TODO: Change this ActorRef to ActorRef<Msg, Void>
    private ActorRef<Msg> userRepo;
    private ActorRef<Msg> roomRepo;
    private ActorRef<String> writer;
    private ActorRef<Msg> handler;
    private ActorRef<Notification> notificationHandler;
    private String currentUser;

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
        handler = (new MessageHandler(currentUser, writer, userRepo, roomRepo, notificationHandler)).spawn();
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

    private void forward(String req) throws SuspendExecution {
        Msg m = new Msg(Msg.commandType(c.command), c.args, self());
        handler.send(m);
    }

    // READ LOOP
    private void readLoop() throws IOException, SuspendExecution {
        // TODO: Exit gracefully sometime in the future
        while(true) {
            forward( read() );
        }
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

    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {
        try {
            init();
            readLoop();
        } catch (IOException e) {
            e.printStackTrace();
        }

        return null;
    }
}
