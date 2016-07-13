package async.echo;

        import async.multi.WorkerHub;

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

    private final AsyncReader readCallback = new AsyncReader();
    private final AsyncWriter writeCallback = new AsyncWriter();

    public AsyncWorker(AsynchronousSocketChannel client) {
        this.client = client;
        buffer = ByteBuffer.allocate(N_BYTES);
    }

    public AsyncWorker(AsynchronousSocketChannel client, int nrBytes) {
        this.client = client;
        buffer = ByteBuffer.allocate(nrBytes);
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
            } catch (IOException e) { /* we couldn't close it. Oh well... */ }
        client.write(buffer, this, writeCallback);
    }

    public void exception(Throwable t) {
        try {
            client.close();
        } catch (IOException e) {}
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
}
