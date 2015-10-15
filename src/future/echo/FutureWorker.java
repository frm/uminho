package future.echo;

import spullara.nio.channels.FutureSocketChannel;

import java.nio.ByteBuffer;
import java.util.concurrent.CompletableFuture;

/**
 * Created by frm on 15/10/15.
 */
public class FutureWorker {
    private FutureSocketChannel client;
    private ByteBuffer buffer;

    public static final int DEFAULT_SIZE = 1024;

    public FutureWorker(FutureSocketChannel socket) {
        client = socket;
        buffer = ByteBuffer.allocate(DEFAULT_SIZE);
    }

    public FutureWorker(FutureSocketChannel socket, int size) {
        client = socket;
        buffer = ByteBuffer.allocate(size);
    }

    public CompletableFuture<Void> handle() {
        return read();
    }

    private CompletableFuture<Void> read() {
        // reading the bytes, placing them in the ByteBuffer, returning the nr of bytes read
        CompletableFuture<Integer> readBytes = client.read(buffer);

        // thenCompose will take action after we read from the client
        return readBytes.thenCompose(this::readCallback);
    }

    private CompletableFuture<Void> write() {
        // writing the bytes to the client
        // nr of written bytes will be placed in the promise
        CompletableFuture<Integer> writtenBytes = client.write(buffer);
        return writtenBytes.thenCompose(this::writeCallback);
    }

    // called after reading from the socket
    // gets the nr of read bytes as first parameter
    private CompletableFuture<Void> readCallback(int nrBytes) {
        buffer.flip();
        if(!buffer.hasRemaining()) { // if there are no bytes remaining, the client closed the connection
            client.close();
            return CompletableFuture.completedFuture(null); // mark the promise as resolved and don't loop anymore
        }

        return write(); // after reading we want to write to the client what he just said
    }

    // called after we have written to the client
    private CompletableFuture<Void> writeCallback(int nrBytes) {
        buffer.clear();
        return read();
    }
}
