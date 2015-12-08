package server;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import co.paralleluniverse.fibers.Suspendable;
import co.paralleluniverse.fibers.io.FiberSocketChannel;
import communication.Command;
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
    private boolean connected;

    public static final int DEFAULT_SIZE = 1024;

    public LineReader(FiberSocketChannel client, ActorRef<Msg> userRepo, ActorRef<Msg> roomRepo, ActorRef<Notification> nh) {
        this.cl = client;
        this.userRepo = userRepo;
        this.roomRepo = roomRepo;
        this.buf = ByteBuffer.allocate(DEFAULT_SIZE);
        this.notificationHandler = nh;
    }

    private void init() {
        writer = (new LineWriter(cl)).spawn();
        handler = (new MessageHandler(writer, userRepo, roomRepo, notificationHandler)).spawn();
        connected = true;
    }

    private void disconnect() throws SuspendExecution {
        handler.send(new Msg(Msg.Type.DEAUTH, null, self()));
    }

    @Suspendable
    private String read() throws IOException {
        buf.clear();
        cl.read(buf);
        buf.flip();
        return new String(buf.array(), 0, buf.limit());
    }

    private void forward(String req) throws SuspendExecution {
        Command c = Command.parse(req);
        Msg m = new Msg(Msg.commandType(c.command), c.args, self());
        handler.send(m);
        if(m.type == Msg.Type.DEAUTH) {
            connected = false;
        }
    }

    private void readLoop() throws IOException, SuspendExecution {
        while(connected) {
            forward( read() );
        }

        disconnect();
    }

    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {
        try {
            init();
            readLoop();
        } catch (IOException e) {
            // Forcing a disconnect
            disconnect();
        }

        return null;
    }
}
