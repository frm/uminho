package future.ring;

import spullara.nio.channels.FutureServerSocketChannel;
import spullara.nio.channels.FutureSocketChannel;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

/**
 * Created by frm on 15/10/15.
 */
public class Server {
    private FutureServerSocketChannel ss;
    private FutureWorker last;
    private int workerId = 0;
    private int port;

    public final static int DEFAULT_PORT = 3000;

    public Server(int port) {
        this.port = port;
    }

    public Server() {
        this.port = DEFAULT_PORT;
    }

    public void listen() throws IOException, ExecutionException, InterruptedException {
        bind();
        accept().get();
    }

    private void bind() throws IOException {
        if(ss == null)
            ss = new FutureServerSocketChannel();

        ss.bind(new InetSocketAddress(port));
    }

    private CompletableFuture<Void> accept() {
        CompletableFuture<FutureSocketChannel> client = ss.accept();
        client.thenCompose(this::addToRing);
        return client.thenCompose((s) -> accept());
    }

    private boolean emptyRing() {
        return last == null;
    }

    private void initRing(FutureWorker w) {
        last = w;
        last.setNext(w);
        last.setPrev(w);
    }

    private CompletableFuture<Void> addToRing(FutureSocketChannel client) {
        FutureWorker w = new FutureWorker(client, ++workerId);
        if(emptyRing())
            initRing(w);

        w.setPrev(last);
        w.setNext(last.getNext());
        last.setNext(w);
        last = w;

        return w.handle();
    }
}
