package async.ring;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.channels.AsynchronousChannelGroup;
import java.nio.channels.AsynchronousServerSocketChannel;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;
import java.util.concurrent.ThreadFactory;

/**
 * Created by frm on 15/10/15.
 */
public class Server {
    private static final int DEFAULT_PORT = 3000;
    private static final int NR_THREADS = 4;
    private static AsynchronousChannelGroup group;

    static {
        try {
            group = newGroup();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private AsynchronousServerSocketChannel ss;
    private int port;
    private AsyncWorker last; // last worker in the ring
    private int workerId = 0;

    private final Acceptor acceptor = new Acceptor();

    public Server(int port) {
        this.port = port;
    }

    public Server() {
        this.port = DEFAULT_PORT;
    }

    private void bind() throws IOException {
        if(ss == null)
            ss = AsynchronousServerSocketChannel.open();

        ss.bind(new InetSocketAddress(port));
    }

    private void accept() {
        ss.accept(this, acceptor);
    }

    public void listen() throws IOException {
        bind();
        accept();
    }

    public boolean hasClients() {
        return last != null;
    }

    private static AsynchronousChannelGroup newGroup() throws IOException {
        return newGroup(NR_THREADS);
    }

    private static AsynchronousChannelGroup newGroup(int nrThreads) throws IOException {
        ThreadFactory f = new ThreadFactory() {
            @Override
            public Thread newThread(Runnable r) {
                return new Thread(r);
            }
        };

        return AsynchronousChannelGroup.withFixedThreadPool(nrThreads, f);
    }

    class Acceptor implements CompletionHandler<AsynchronousSocketChannel, Server> {
        private void initRing(AsyncWorker w) {
            last = w;
            last.setPrev(w);
            last.setNext(w);
        }

        private void addToRing(Server attachment, AsyncWorker w) {
            if(!attachment.hasClients()) // check if it's the first client
                initRing(w);

            w.setNext(last.getNext());
            w.setPrev(last);
            last.setNext(w);
            last = w;
        }

        @Override
        public void completed(AsynchronousSocketChannel result, Server attachment) {
            AsyncWorker w = new AsyncWorker(result, ++workerId);
            addToRing(attachment, w);
            w.handle();
            attachment.accept();
        }

        @Override
        public void failed(Throwable exc, Server attachment) {}
    }
}
