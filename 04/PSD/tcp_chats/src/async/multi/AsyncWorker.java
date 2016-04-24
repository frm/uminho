package async.multi;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;

/**
 * Created by frm on 14/10/15.
 */
public class AsyncWorker {
    private static final int N_BYTES = 1024;

    private AsynchronousSocketChannel client;
    private ByteBuffer buffer;
    private WorkerHub hub;
    private int id;

    private final AsyncReader readCallback = new AsyncReader();
    private final AsyncWriter writeCallback = new AsyncWriter();
    private final MockCallback mockCallback = new MockCallback();

    public AsyncWorker(AsynchronousSocketChannel client, WorkerHub hub) {
        this.client = client;
        buffer = ByteBuffer.allocate(N_BYTES);
        this.hub = hub;
        this.id = hub.subscribe(this);
    }

    public AsyncWorker(AsynchronousSocketChannel client, int nrBytes, WorkerHub hub) {
        this.client = client;
        buffer = ByteBuffer.allocate(nrBytes);
        this.hub = hub;
        this.id = hub.subscribe(this);
    }

    // I know we are just calling read, but I don't want to expose AsyncWorker#read as a public API method
    public void handle() {
        read();
    }

    // We are going to start reading. Clear the buffer and call the async read method
    // Second parameter is the attachment, we are sending the current instance since we want to be able
    // to call the write callback with readCallback
    private void read() {
        buffer.clear();
        client.read(buffer, this, readCallback);
    }

    // We have just read from the socket
    // flip the buffer. if it has none, we read 0 bytes and it's time to close the connection
    // otherwise, write to the socket what the client just said
    private void write() {
        buffer.flip();
        if(!buffer.hasRemaining()) // if the client closed the connection and we read 0 bytes
            try {
                client.close(); // try to close the client
                hub.unsubscribe(id); // remove ourselves from the set of client handlers
            } catch (IOException e) { /* we couldn't close it. Oh well... */ }
        hub.diffuse(id, buffer, writeCallback); // send the message to everyone
    }

    public void send(ByteBuffer buf, int senderId) {
        // only send to our client if the message is not his own
        // after writing we don't want to do anything since we already have a write callback set up
        // in hub.diffuse(..., writeCallback). That is going to be called after we send a message to everyone
        // yet, we need to send in a callback
        // So I just made a mock callback that doesn't do anything
        if(senderId != id)
            client.write(buf, null, mockCallback);
    }

    public void exception(Throwable t) {
        try {
            client.close();
            hub.unsubscribe(id);
        } catch (IOException e) {}
    }

    // See WorkerHub#diffuse
    public static ByteBuffer cloneByteBuffer(ByteBuffer original) {
        ByteBuffer clone = ByteBuffer.allocate(original.capacity());
        original.rewind();
        clone.put(original);
        original.rewind();
        clone.flip();
        return clone;
    }

    // These are just simple callback implementations
    // We are saving a singleton in variables
    // They will always receive their parent and just call the corresponding method
    // Since after reading (readCallback about to be executed) we are always going to write
    class AsyncReader implements CompletionHandler<Integer, AsyncWorker> {
        @Override
        public void completed(Integer bytes, AsyncWorker parent) {
            parent.write();
        }

        @Override
        public void failed(Throwable exc, AsyncWorker parent) {
            parent.exception(exc);
        }
    }

    // After writing we are going to execute the write callback and we will always want to read
    class AsyncWriter implements CompletionHandler<Integer, AsyncWorker> {
        @Override
        public void completed(Integer bytes, AsyncWorker parent) {
            parent.read();
        }

        @Override
        public void failed(Throwable exc, AsyncWorker parent) {
            parent.exception(exc);
        }
    }

    // See AsyncWorker#send
    class MockCallback implements CompletionHandler<Integer, AsyncWorker> {

        @Override
        public void completed(Integer result, AsyncWorker attachment) {}

        @Override
        public void failed(Throwable exc, AsyncWorker attachment) {}
    }
}
