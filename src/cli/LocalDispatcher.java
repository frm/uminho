package cli;

import server.Server;

import java.io.IOException;

public class LocalDispatcher extends Dispatcher {
    private Server server;
    private Pipe pipe;

    LocalDispatcher(Integer p) throws IOException {
        super(p);
        pipe = new Pipe();

        server = new Server(p, pipe.reversed());

        this.port = null;
    }

    @Override
    public void terminate() {

    }
}
