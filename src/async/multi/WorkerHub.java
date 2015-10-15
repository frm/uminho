package async.multi;

import java.nio.ByteBuffer;
import java.nio.channels.CompletionHandler;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by frm on 15/10/15.
 */
public class WorkerHub {
    // The subscriber list is a set of workers that want to receive the messages
    private Map<Integer, AsyncWorker> subscribers;
    private int subscriberCount;

    public WorkerHub() {
        this.subscribers = new HashMap<Integer, AsyncWorker>();
        this.subscriberCount = 0;
    }

    public int subscribe(AsyncWorker w) {
        int id = ++subscriberCount;
        subscribers.put(id, w);
        return id;
    }

    public boolean unsubscribe(int id) {
        boolean b = subscribers.containsKey(id);

        if(b)
            subscribers.remove(id);

        return b;
    }

    public void diffuse(int senderId, ByteBuffer buffer, CompletionHandler<Integer, AsyncWorker> writeCallback) {
        for(AsyncWorker w : subscribers.values()) {
            // We need to make a copy because we want to send the same message to every worker
            // Yet, using the write method from AsynchronousSocketChannel, changes the buffer we give him
            // So we copy it into a new variable each time
            // This is why I hate shared state.
            // No immutability means nasty side effects
            // And Java can't give us that guarantee.
            ByteBuffer copy = AsyncWorker.cloneByteBuffer(buffer);
            w.send(copy, senderId);
        }

        // when a client shutsdown, it sends null value
        // and we get a null pointer exception here
        // so, just double check
        AsyncWorker sender = subscribers.get(senderId);
        if(sender != null)
            writeCallback.completed(null, sender);
    }
}
