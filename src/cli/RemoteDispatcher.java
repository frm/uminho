package cli;

import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;

public class RemoteDispatcher extends Dispatcher {
    Socket clientSocket;
    ObjectOutputStream out;

    RemoteDispatcher(Integer p) throws IOException {
        super(p);

        clientSocket = new Socket("127.0.0.1", p);
        out = new ObjectOutputStream(clientSocket.getOutputStream());

        fetcher = new RemoteFetcher(clientSocket);
        new Thread(fetcher).start();
    }

    public void terminate(){
        try {
            clientSocket.shutdownOutput(); // Sends the 'FIN' on the network
        } catch (Exception e) {} // for when the stream is somehow damaged

        try {
            InputStream is = clientSocket.getInputStream(); // obtain stream
            while (is.read() >= 0) ; // "read()" returns '-1' when the 'FIN' is reached
        } catch (Exception e) {} // for when the stream is somehow damaged

        try {
            clientSocket.close(); // Now we can close the Socket
        } catch (Exception e) {} // for when something is somehow damaged

        clientSocket = null; //now it's closed!
    }

    @Override
    void send(Object obj) throws IOException {
        out.writeObject(obj);
    }
}
