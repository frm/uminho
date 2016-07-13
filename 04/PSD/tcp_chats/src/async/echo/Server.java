package async.echo;

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
    private static final int DEFAULT_PORT = 8000;
    private static AsynchronousChannelGroup group;

    // lol really ugly code, sorry about this
    static {
        try {
            group = newGroup();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private AsynchronousServerSocketChannel ss;
    private int port;

    private final Acceptor acceptor = new Acceptor();

    public Server(int port) throws IOException {
        this.port = port;
    }

    public Server() throws IOException {
        this.port = DEFAULT_PORT;
    }

    public void listen() throws IOException {
        bind();
        accept();
    }

    private void accept() {
        // accepting a client
        // the first parameter is the attachment given to the client when it is accepted
        // we pass the parent because the acceptor will create a new worker and then just call accept again
        // the second parameter is the completion handler for the callback
        // we are using the acceptor bound to this server instance
        ss.accept(this, acceptor);
    }

    // binding the async server socket to the port
    private void bind() throws IOException {
        if(ss == null)
            ss = AsynchronousServerSocketChannel.open();

        ss.bind(new InetSocketAddress(port));
    }

    // Creating a new Async Channel Group using a set number of threads
    public static AsynchronousChannelGroup newGroup(int threadNr) throws IOException {
        ThreadFactory factory = new ThreadFactory() {
            @Override
            public Thread newThread(Runnable r) {
                return new Thread(r);
            }
        };

        return AsynchronousChannelGroup.withFixedThreadPool(threadNr, factory);
    }

    public static AsynchronousChannelGroup newGroup() throws IOException {
        return Server.newGroup(THREAD_NR);
    }

    class Acceptor implements CompletionHandler<AsynchronousSocketChannel, Server> {
        @Override
        public void completed(AsynchronousSocketChannel client, Server parent) {
            new AsyncWorker(client).handle();
            parent.accept();
        }

        @Override
        public void failed(Throwable exc, Server parent) {
            group.shutdown(); // if we can't accept a client, throw the group away
        }
    }
}
