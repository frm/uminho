package async.multi;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.channels.AsynchronousChannelGroup;
import java.nio.channels.AsynchronousServerSocketChannel;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;
import java.util.concurrent.ThreadFactory;

/**
 * Created by frm on 14/10/15.
 */
public class Server {
    private static final int THREAD_NR = 2;
    private static final int DEFAULT_PORT = 3000;
    private static AsynchronousChannelGroup group;

    // lol, i'm so sorry for this...
    static {
        try {
            group = newGroup();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private int port;
    private WorkerHub hub;
    private AsynchronousServerSocketChannel ss;

    // Instead of creating a private acceptor class, since it's a singleton, just keep it in this variable
    private CompletionHandler<AsynchronousSocketChannel, Server> acceptor = new CompletionHandler<AsynchronousSocketChannel, Server>() {
        @Override
        public void completed(AsynchronousSocketChannel client, Server parent) {
            new AsyncWorker(client, hub).handle();
            parent.accept();
        }

        @Override
        public void failed(Throwable exc, Server parent) {
            group.shutdown();
        }
    };

    public Server() {
        this.port = DEFAULT_PORT;
        this.hub = new WorkerHub();
    }

    public Server(int port) {
        this.port = port;
        this.hub = new WorkerHub();
    }

    public void bind() throws IOException {
        if(ss == null)
            ss = AsynchronousServerSocketChannel.open();

        ss.bind(new InetSocketAddress(port));
    }

    public void accept() {
        ss.accept(this, acceptor);
    }

    public void listen() throws IOException {
        bind();
        accept();
    }

    public static AsynchronousChannelGroup newGroup() throws IOException {
        return Server.newGroup(THREAD_NR);
    }

    public static AsynchronousChannelGroup newGroup(int nrThreads) throws IOException {
        ThreadFactory f = new ThreadFactory() {
            @Override
            public Thread newThread(Runnable r) {
                return new Thread(r);
            }
        };

        return AsynchronousChannelGroup.withFixedThreadPool(nrThreads, f);
    }
}
