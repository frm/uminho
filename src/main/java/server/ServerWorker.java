package server;

import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import co.paralleluniverse.fibers.io.FiberSocketChannel;
import util.Pair;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedChannelException;

/**
 * Created by frm on 28/11/15.
 */
public class ServerWorker extends BasicActor {
    private final FiberSocketChannel cl;
    private final ByteBuffer buf;
    private final UserRepo repo;

    public static final int DEFAULT_SIZE = 1024;

    public ServerWorker(FiberSocketChannel client, UserRepo users) {
        cl = client;
        repo = users;
        buf = ByteBuffer.allocate(DEFAULT_SIZE);
    }

    public ServerWorker(FiberSocketChannel client, UserRepo users, int bufferCapacity) {
        cl = client;
        repo = users;
        buf = ByteBuffer.allocate(bufferCapacity);
    }

    private Pair<Boolean, String> onConnection(String req) {
        // parse request
        // try to authenticate, register or something
        // Command c = CommandParser.parse(req)
        return new Pair<>(false, req);
    }

    @Override
    protected Object doRun() throws InterruptedException, SuspendExecution {
        boolean connected = false;

        try {
            while(!connected) {
                buf.clear();
                cl.read(buf);
                buf.flip();

                // this should return a pair of boolean and string
                // the string should be sent to the user, may be an error or the channel list
                String req = new String(buf.array(), 0, buf.limit());
                Pair<Boolean, String> p = onConnection(req);
                connected = p.first;
                buf.clear();
                buf.put(p.second.getBytes());
                buf.flip();
                cl.write(buf);
            }
        } catch (IOException e) {
            // TODO: handle client disconnection here
            // if (!connected) dont do anything
            // else go on user list and disconnect
            e.printStackTrace();
        }

        return null;
    }
}
