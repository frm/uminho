package cli;

import packet.*;
import server.Server;

import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;

// Sends queries to server, through proxy or not
public class Dispatcher {
    Boolean isServer;
    Integer port;
    Server server;
    Socket clientSocket;
    Fetcher fetcher;

    // start in client mode and connect to server through socket
    Dispatcher(Integer port) throws IOException {
        isServer = false;
        this.port = port;
        this.server = null;
        clientSocket = new Socket("127.0.0.1", port);
        fetcher = new Fetcher(clientSocket);
        new Thread(fetcher).start();
    }

    // start in server mode and start a new server
    Dispatcher(Server server){
        isServer = true;
        this.port = null;
        this.server = server;
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

    public CreateTaskType doCreateTaskType(CreateTaskType obj) {

    }

    public StartTask doStartTask(StartTask obj) {
        if(isServer){
            //return server.doStartTask(obj);
        }else{
            Receiver<StartTask> r = new Receiver<>(fetcher);
            return r.get();
        }
    }

    public FinishTask doFinishTask(FinishTask obj) {

    }

    public ListAll doListAll(ListAll obj) {

    }

    public Login doLogin(Login obj) {

    }

    public Store doStore(Store obj) {

    }

    public void doSubscribe(Subscribe obj){

    }
}
