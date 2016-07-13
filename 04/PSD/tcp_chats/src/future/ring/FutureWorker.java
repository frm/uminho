package future.ring;

import spullara.nio.channels.FutureSocketChannel;

import java.nio.ByteBuffer;
import java.util.concurrent.CompletableFuture;

/**
 * Created by frm on 15/10/15.
 */
public class FutureWorker {
    private int id;
    private FutureSocketChannel client;
    private ByteBuffer buffer;
    private FutureWorker prev;
    private FutureWorker next;

    public static final int DEFAULT_SIZE = 1024;

    public FutureWorker(FutureSocketChannel socket, int id) {
        this.id = id;
        this.client = socket;
        buffer = ByteBuffer.allocate(DEFAULT_SIZE);
    }

    public FutureWorker(FutureSocketChannel socket, int size, int id) {
        this.id = id;
        this.client = socket;
        buffer = ByteBuffer.allocate(size);
    }

    public FutureWorker getPrev() {
        return prev;
    }

    public void setPrev(FutureWorker prev) {
        this.prev = prev;
    }

    public FutureWorker getNext() {
        return next;
    }

    public void setNext(FutureWorker next) {
        this.next = next;
    }

    public CompletableFuture<Void> handle() {
        return read();
    }

    private CompletableFuture<Void> read() {
        return client.read(buffer).thenCompose(this::readCallback);
    }

    private CompletableFuture<Void> write(Message m) {
        ByteBuffer b = m.safeBuffer();

        if(m.id != id) {
            next.write(m);
            return client.write(b).thenCompose(this::mockCallback);
        }

        buffer.clear();
        return read();
    }

    private CompletableFuture<Void> readCallback(int nrBytes) {
        buffer.flip();
        if(!buffer.hasRemaining()) {
            leaveRing();
            return CompletableFuture.completedFuture(null);
        }

        return next.write(new Message(buffer, id));
    }

    private CompletableFuture<Void> mockCallback(int nrBytes) {
        return CompletableFuture.completedFuture(null);
    }

    private void leaveRing() {
        client.close();
        next.setPrev(prev);
        prev.setNext(next);
    }
}
