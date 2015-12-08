package server;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import co.paralleluniverse.fibers.Suspendable;
import co.paralleluniverse.fibers.io.FiberSocketChannel;
import communication.Msg;

import java.io.IOException;
import java.nio.ByteBuffer;

/**
 * Created by frm on 06/12/15.
 */
public class LineWriter extends BasicActor<String, Void> {
    private final FiberSocketChannel cl;
    private final ByteBuffer buf;

    public LineWriter(FiberSocketChannel client) {
        cl = client;
        buf = ByteBuffer.allocate(LineReader.DEFAULT_SIZE);
    }

    @Suspendable
    private void write(String s) throws IOException {
        buf.clear();
        buf.put((s + "\n").getBytes()); // STOP. HAMMER TIME
        buf.flip();
        cl.write(buf);
    }

    private void receiveLoop() throws InterruptedException, SuspendExecution {
        while(
            receive(str -> {
                if(str == null) {
                    return false;
                }
                try {
                    write(str);
                } catch (IOException e) {
                    return false;
                }
                return true;
            })
        );
    }

    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {
        receiveLoop();
        return null;
    }
}
