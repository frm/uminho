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

    public CreateTaskType doCreateTaskType(CreateTaskType obj){
        Receiver<CreateTaskType> r = new Receiver<>(fetcher);
        try {
            send(obj);
        } catch (IOException e) {
            obj.r_errors.add("Could not send the create task type packet.");
            return obj;
        }
        return r.get();
    }

    public StartTask doStartTask(StartTask obj){
        Receiver<StartTask> r = new Receiver<>(fetcher);
        try {
            send(obj);
        } catch (IOException e) {
            obj.r_errors.add("Could not send the start task packet.");
            return obj;
        }
        return r.get();
    }

    public FinishTask doFinishTask(FinishTask obj){
        Receiver<FinishTask> r = new Receiver<>(fetcher);
        try {
            send(obj);
        } catch (IOException e) {
            obj.r_errors.add("Could not send the finish task packet.");
            return obj;
        }
        return r.get();
    }

    public ListAll doListAll(ListAll obj){
        Receiver<ListAll> r = new Receiver<>(fetcher);
        try {
            send(obj);
        } catch (IOException e) {
            obj.r_errors.add("Could not send the listAll packet.");
            return obj;
        }
        return r.get();
    }

    public Login doLogin(Login obj){
        Receiver<Login> r = new Receiver<>(fetcher);
        try {
            send(obj);
        } catch (IOException e) {
            obj.r_errors.add("Could not send the login packet.");
            return obj;
        }
        return r.get();
    }

    public Store doStore(Store obj){
        Receiver<Store> r = new Receiver<>(fetcher);
        try {
            send(obj);
        } catch (IOException e) {
            obj.r_errors.add("Could not send the store packet.");
            return obj;
        }
        return r.get();
    }

    public Boolean doSubscribe(Subscribe obj) {
        Receiver<Subscribe> r = new Receiver<>(fetcher);
        Thread t = new Thread(new Subscriber(r));
        t.start();
        try {
            send(obj);
        } catch (IOException e) {
            t.interrupt();
            return false;
        }
        return true;
    }
}
