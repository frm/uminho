package cli;

import packet.*;
import warehouse.UnknownPacketException;

import java.io.EOFException;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.net.Socket;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;

public abstract class Fetcher implements Runnable{
    private static Map<Integer, Receiver> receivers;
    private static ReentrantLock receiversLock;
    private ObjectInputStream in;

    public void addReceiver(Integer id, Receiver receiver){
        receiversLock.lock();
        receivers.put(id, receiver);
        receiversLock.unlock();
    }

    Fetcher(ObjectInputStream in){
        this.in = in;
        receiversLock = new ReentrantLock();
        receivers = new HashMap<>();
    }

    abstract Boolean streamIsOK();

    @Override
    public void run() {
        try {
            Integer id;

            while(streamIsOK()){
                try {
                    Object obj = in.readObject();

                    if(obj instanceof CreateTaskType){
                        CreateTaskType o = (CreateTaskType) obj;
                        receiversLock.lock();
                        receivers.get(o.id).set(o);
                        receiversLock.unlock();
                    } else if(obj instanceof FinishTask){
                        FinishTask o = (FinishTask) obj;
                        receiversLock.lock();
                        receivers.get(o.id).set(o);
                        receiversLock.unlock();
                    } else if(obj instanceof ListAll){
                        ListAll o = (ListAll) obj;
                        receiversLock.lock();
                        receivers.get(o.id).set(o);
                        receiversLock.unlock();
                    } else if(obj instanceof Login){
                        Login o = (Login) obj;
                        receiversLock.lock();
                        receivers.get(o.id).set(o);
                        receiversLock.unlock();
                    } else if(obj instanceof StartTask){
                        StartTask o = (StartTask) obj;
                        receiversLock.lock();
                        receivers.get(o.id).set(o);
                        receiversLock.unlock();
                    } else if(obj instanceof Store){
                        Store o = (Store) obj;
                        receiversLock.lock();
                        receivers.get(o.id).set(o);
                        receiversLock.unlock();
                    } else if(obj instanceof Subscribe){
                        Subscribe o = (Subscribe) obj;
                        receiversLock.lock();
                        receivers.get(o.id).set(o);
                        receiversLock.unlock();
                    } else if (obj instanceof Packet) {
                        Packet o = (Packet) obj;
                        receiversLock.lock();
                        receivers.get(o.id).set(o);
                        receiversLock.unlock();
                    } else {
                        throw new UnknownPacketException("Fetcher got an unexpected packet.");
                    }
                } catch (ClassNotFoundException | UnknownPacketException e) {
                    e.printStackTrace();
                } catch (IOException e) {
                    throw e; //send it to the outer try block
                } catch (Exception e){
                    e.printStackTrace();
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
