package server;

import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import co.paralleluniverse.fibers.io.FiberServerSocketChannel;

import java.io.IOException;
import java.net.InetSocketAddress;

/**
 * Created by frm on 28/11/15.
 */
public class Server extends BasicActor {
    private FiberServerSocketChannel ss;
    private UserRepo users;
    private int port;

    private final static int DEFAULT_PORT = 3000;

    public Server() {
        port = DEFAULT_PORT;
        users = new UserRepo();
    }

    public Server(int port) {
        this.port = port;
        users = new UserRepo();
    }

    public void bind() throws IOException, SuspendExecution {
        if(ss == null)
            ss = FiberServerSocketChannel.open();

        ss.bind(new InetSocketAddress(port));
    }

    public void accept() throws IOException, SuspendExecution {
        new ServerWorker(ss.accept(), users).spawn();
    }

    @Override
    protected Object doRun() throws InterruptedException, SuspendExecution {
        try {
            bind();
            for(;;) accept();
        }
        catch (IOException e) {
            return null;
        }
    }
}
