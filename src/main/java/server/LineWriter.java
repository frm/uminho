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
        buf.put(s.getBytes());
        buf.flip();
        cl.write(buf);
    }

    private void receiveLoop() throws InterruptedException, SuspendExecution {
        boolean inRoom = false;
        //TODO: Use inRoom

        while (
                receive(msg -> {
                    ActorRef sender = msg.sender; // @jorod: I don't think this is needed
                    System.out.println(msg.content);

                    try {
                        switch (msg.type) {
                            case ROOMS:
                                write( msg.content.toString());
                                return true;
                            //FROM ROOM @jorod: I don't understand this comment
                            case NEW_CHAT:
                                write( msg.content.toString());
                                return true;
                            case ROOM_USERS:
                                write( msg.content.toString());
                                return true;
                            case KICK:
                                write( "The room was closed, or you were kicked from it"); // @jorod: this should use the config file
                                return true;
                            case OK:
                                write( msg.content.toString());
                                return true;
                        }
                    } catch (IOException e) {
                        e.printStackTrace();
                    }

                    return false;
                })) ;
    }

    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {
        receiveLoop();
        return null;
    }
}
