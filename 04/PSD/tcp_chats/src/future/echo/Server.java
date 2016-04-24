package future.echo;

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
    private int port;

    private static final int DEFAULT_PORT = 3000;

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
        // accept a client
        CompletableFuture<FutureSocketChannel> client = ss.accept();

        // when the client is accepted, somewhere in the future, we need to handle it
        // thenCompose takes a function, that gives the result of accept as a parameter
        // which is a FutureSocketChannel.
        // We create a new FutureWorker and set it to handle that
        client.thenCompose((s) -> new FutureWorker(s).handle());

        // when the client is accepted we loop back this function
        return client.thenCompose((s) -> accept());
    }
}
