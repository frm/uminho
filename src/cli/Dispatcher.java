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
        obj.id = r.id;
        try {
            send(obj);
        } catch (IOException e) {
            obj.r_errors.add("Could not send the create task type packet.");
            return obj;
        }

        Packet p = r.get();
        CreateTaskType ret;
        if(! (p instanceof CreateTaskType) ) {
            ret = new CreateTaskType();
            ret.r_errors = p.r_errors;
        } else {
            ret = (CreateTaskType) p;
        }

        return ret;
    }

    public StartTask doStartTask(StartTask obj){
        Receiver<StartTask> r = new Receiver<>(fetcher);
        obj.id = r.id;
        try {
            send(obj);
        } catch (IOException e) {
            obj.r_errors.add("Could not send the start task packet.");
            return obj;
        }
        Packet p = r.get();
        StartTask ret;
        if(! (p instanceof StartTask) ) {
            ret = new StartTask();
            ret.r_errors = p.r_errors;
        } else {
            ret = (StartTask) p;
        }

        return ret;
    }

    public FinishTask doFinishTask(FinishTask obj){
        Receiver<FinishTask> r = new Receiver<>(fetcher);
        obj.id = r.id;
        try {
            send(obj);
        } catch (IOException e) {
            obj.r_errors.add("Could not send the finish task packet.");
            return obj;
        }
        Packet p = r.get();
        FinishTask ret;
        if(! (p instanceof FinishTask) ) {
            ret = new FinishTask();
            ret.r_errors = p.r_errors;
        } else {
            ret = (FinishTask) p;
        }

        return ret;
    }

    public ListAll doListAll(ListAll obj){
        Receiver<ListAll> r = new Receiver<>(fetcher);
        obj.id = r.id;
        try {
            send(obj);
        } catch (IOException e) {
            obj.r_errors.add("Could not send the listAll packet.");
            return obj;
        }
        Packet p = r.get();
        ListAll ret;
        if(! (p instanceof ListAll) ) {
            ret = new ListAll();
            ret.r_errors = p.r_errors;
        } else {
            ret = (ListAll) p;
        }

        return ret;
    }

    public Login doLogin(Login obj){
        Receiver<Login> r = new Receiver<>(fetcher);
        obj.id = r.id;
        try {
            send(obj);
        } catch (IOException e) {
            obj.r_errors.add("Could not send the login packet.");
            return obj;
        }
        Packet p = r.get();
        Login ret;
        if(! (p instanceof Login) ) {
            ret = new Login();
            ret.r_errors = p.r_errors;
        } else {
            ret = (Login) p;
        }

        return ret;
    }

    public Store doStore(Store obj){
        Receiver<Store> r = new Receiver<>(fetcher);
        obj.id = r.id;
        try {
            send(obj);
        } catch (IOException e) {
            obj.r_errors.add("Could not send the store packet.");
            return obj;
        }
        Packet p = r.get();
        Store ret;
        if(! (p instanceof Store) ) {
            ret = new Store();
            ret.r_errors = p.r_errors;
        } else {
            ret = (Store) p;
        }

        return ret;
    }

    public Boolean doSubscribe(Subscribe obj) {
        Receiver<Subscribe> r = new Receiver<>(fetcher);
        obj.id = r.id;
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
