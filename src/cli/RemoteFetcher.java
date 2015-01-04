package cli;


import java.io.IOException;
import java.io.ObjectInputStream;
import java.net.Socket;

public class RemoteFetcher extends Fetcher {
    Socket clientSocket;

    RemoteFetcher(Socket clientSocket) throws IOException {
        super(new ObjectInputStream(clientSocket.getInputStream()));
        this.clientSocket = clientSocket;
    }

    @Override
    Boolean streamIsOK() {
        return !clientSocket.isClosed();
    }
}
