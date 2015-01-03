package cli;

import server.Server;

import java.io.IOException;

public class LocalDispatcher extends Dispatcher {
    private Server server;
    private Pipe pipe;

    LocalDispatcher(Integer p) throws IOException {
        super(p);
        pipe = new Pipe();
        fetcher = new LocalFetcher(pipe);
        server = new Server(p, pipe.reversed());
        new Thread(fetcher).start();
    }

    @Override
    public void terminate() {
        pipe.close();
        server.stop();
    }

    @Override
    void send(Object obj) throws IOException {
        pipe.write(obj);
    }
}
