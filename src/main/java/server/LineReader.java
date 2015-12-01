package server;

import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import co.paralleluniverse.fibers.io.FiberSocketChannel;
import util.Pair;

import java.io.IOException;
import java.nio.ByteBuffer;

/**
 * Created by frm on 28/11/15.
 */
public class LineReader extends BasicActor {
    private final FiberSocketChannel cl;
    private final ByteBuffer buf;
    private final UserRepo repo;
    private String currentUser;

    public static final int DEFAULT_SIZE = 1024;

    public LineReader(FiberSocketChannel client, UserRepo users) {
        cl = client;
        repo = users;
        buf = ByteBuffer.allocate(DEFAULT_SIZE);
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
            reply = "User successfully created!";
        }
        else {
            // TODO: move this to a config file
            reply = "Error creating account";
        }
        return new Pair<>(res, reply);
    }

    private Pair<Boolean, String> authenticateUser(String uname, String password) {
        Boolean res = repo.logIn(uname, password, self());
        String reply = null;
        if(res) {
            // reply = TODO: list of channels here
            currentUser = uname;
            reply = "User successfully authenticated!";
        }
        else {
            // TODO: move this to a config file
            reply = "Invalid auth credentials";
        }
        return new Pair<>(res, reply);
    }

    private Pair<Boolean, String> deleteUser(String uname, String password) {
        Boolean res = repo.delete(uname, password);
        String reply = null;
        if(res) {
            reply = "User successfully deleted!";
        }
        else {
            // TODO: move this to a config file
            reply = "Invalid auth credentials";
        }

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
                return new Pair<>(false, "Invalid command");
        }
    }

    private String read() throws IOException {
        buf.clear();
        cl.read(buf);
        buf.flip();
        return new String(buf.array(), 0, buf.limit());
    }

    private void write(String s) throws IOException {
        buf.clear();
        buf.put(s.getBytes());
        buf.flip();
        cl.write(buf);
    }

    @Override
    protected Object doRun() throws InterruptedException, SuspendExecution {
        boolean connected = false;

        try {
            while(!connected) {
                // TODO: instrumentalize this section
                // String req = read();

                buf.clear();
                cl.read(buf);
                buf.flip();
                String req = new String(buf.array(), 0, buf.limit());

                Pair<Boolean, String> p = onConnection(req);

                connected = p.first;

               // write(p.second);
                buf.clear();
                buf.put(p.second.getBytes());
                buf.flip();
                cl.write(buf);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        if(connected)
            repo.disconnect(currentUser, self());

        return null;
    }
}
