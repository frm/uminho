package cli;

import packet.*;
import server.Server;

import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;

public abstract class Dispatcher {
    Integer port;
    Fetcher fetcher;

    Dispatcher(Integer port) {
        this.port = port;
    }

    public abstract void terminate();

    abstract void send(Object obj) throws IOException;

    /*
    ~~doStartTask example:
    if(isServer){
        //return server.doStartTask(obj);
    }else{
        Receiver<StartTask> r = new Receiver<>(fetcher);
        return r.get();
    }
    */

    public CreateTaskType doCreateTaskType(CreateTaskType obj){
        return null;
    }

    public StartTask doStartTask(StartTask obj){
        return null;
    }

    public FinishTask doFinishTask(FinishTask obj){
        return null;
    }

    public ListAll doListAll(ListAll obj){
        return null;
    }

    public Login doLogin(Login obj){
        return null;
    }

    public Store doStore(Store obj){
        return null;
    }

    public void doSubscribe(Subscribe obj) {

    }
}
