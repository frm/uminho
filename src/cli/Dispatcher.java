package cli;

import packet.*;
import server.Server;

// Sends queries to server, through proxy or not
public class Dispatcher {
    Boolean isServer;
    Integer port;
    Server server;

    // start in client mode and connect to server through socket
    Dispatcher(Integer port){
        isServer = false;
        this.port = port;
        this.server = null;
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
            //TODO: close the socket
        }
    }

    public void doCreateTaskType(CreateTaskType obj) {

    }

    public void doStartTask(StartTask obj) {

    }

    public void doFinishTask(FinishTask obj) {

    }

    public void doListAll(ListAll obj) {

    }

    public void doLogin(Login obj) {

    }

    public void doStore(Store obj) {

    }

    public void doSubscribe(Subscribe obj){

    }
}
