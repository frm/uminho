package cli;


import java.io.ObjectInputStream;

public class LocalFetcher extends Fetcher {
    Pipe pipe;

    LocalFetcher(Pipe pipe) {
        super(pipe.getIn());
        this.pipe = pipe;
    }

    @Override
    Boolean streamIsOK() {
        return !pipe.isClosed();
    }
}
