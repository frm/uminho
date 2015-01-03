package cli;

import java.io.InputStream;
import java.net.Socket;

public class RemoteDispatcher extends Dispatcher {
    Socket clientSocket;
    Fetcher fetcher;

    RemoteDispatcher(Integer p){
        super(p);

        clientSocket = new Socket("127.0.0.1", port);
        fetcher = new Fetcher(clientSocket);
        new Thread(fetcher).start();
    }

    public void terminate(){
        if(isServer){
            server.stop();
        }else{
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
    }
}
