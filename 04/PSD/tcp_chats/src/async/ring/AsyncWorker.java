package async.ring;

import javax.annotation.processing.Completion;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;

/**
 * Created by frm on 15/10/15.
 */
public class AsyncWorker {
    private int id;
    private AsyncWorker next;
    private AsyncWorker prev;
    private AsynchronousSocketChannel client;
    private ByteBuffer buffer;

    private final AsyncReader readCallback = new AsyncReader();
    private final AsyncWriter writeCallback = new AsyncWriter();
    private final MockCallback mockCallback = new MockCallback();

    private static final int DEFAULT_SIZE = 1024;

    public AsyncWorker(AsynchronousSocketChannel result, int id) {
        this.client = result;
        this.id = id;
        this.buffer = ByteBuffer.allocate(DEFAULT_SIZE);
    }

    public AsyncWorker(AsynchronousSocketChannel result, int size, int id) {
        this.client = result;
        this.id = id;
        this.buffer = ByteBuffer.allocate(size);
    }

    public void setNext(AsyncWorker w) {
        next = w;
    }

    public void setPrev(AsyncWorker w) {
        prev = w;
    }

    public AsyncWorker getNext() {
        return next;
    }

    public AsyncWorker getPrev() {
        return prev;
    }

    public void handle() {
        read();
    }

    private void read() {
        buffer.clear();
        client.read(buffer, this, readCallback);
    }

    private void write() {
        buffer.flip();
        if(!buffer.hasRemaining()) {
            try {
                leaveRing();
            } catch (IOException e) { /* Couldn't close, ignore */ }
        }

        // disconnecting sends a message of size 0
        // if we send one of those and then leave the chain
        // the message will be passed around infinitely
        // because it will never reach its sender
        // so we only send the message if its size isn't 0
        if(buffer.limit() != 0)
            next.send(new Message(buffer, id), writeCallback); // send the message to the next worker in the ring
    }

    private void send(Message m, AsyncWriter callback) {
        if(m.id == id) // we reached a full lap, time to do the callback
            callback.completed(null, this);
        else { // we haven't reached a full lap, write the message and call the next worker
            client.write(m.safeBuffer(), null, mockCallback);
            next.send(m, callback);
        }
    }

    private void leaveRing() throws IOException {
        client.close();
        // Removing from the callback chain
        prev.setNext(next);
        next.setPrev(prev);
    }

    public void exception(Throwable exc) {
        try {
            leaveRing();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    class AsyncReader implements CompletionHandler<Integer, AsyncWorker> {
        @Override
        public void completed(Integer result, AsyncWorker parent) {
            parent.write();
        }

        @Override
        public void failed(Throwable exc, AsyncWorker parent) {
            parent.exception(exc);
        }
    }

    class AsyncWriter implements CompletionHandler<Integer, AsyncWorker> {
        @Override
        public void completed(Integer result, AsyncWorker parent) {
            parent.read();
        }

        @Override
        public void failed(Throwable exc, AsyncWorker parent) {
            parent.exception(exc);
        }
    }

    class MockCallback implements CompletionHandler<Integer, Void> {

        @Override
        public void completed(Integer result, Void attachment) {}

        @Override
        public void failed(Throwable exc, Void attachment) {}
    }
}
