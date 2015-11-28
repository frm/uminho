package server;

import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import co.paralleluniverse.fibers.io.FiberSocketChannel;

/**
 * Created by frm on 28/11/15.
 */
public class ServerWorker extends BasicActor {
    public ServerWorker(FiberSocketChannel client) {
    }

    @Override
    protected Object doRun() throws InterruptedException, SuspendExecution {
        return null;
    }
}
