package cli;

import packet.*;
import warehouse.UnknownPacketException;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.net.Socket;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;

public class Fetcher implements Runnable{
    Socket clientSocket;
    private static Map<Integer, Receiver> receivers;
    private static ReentrantLock receiversLock;

    public void addReceiver(Integer id, Receiver receiver){
        receiversLock.lock();
        receivers.put(id, receiver);
        receiversLock.unlock();
    }

    Fetcher(Socket clientSocket){
        this.clientSocket = clientSocket;
    }

    @Override
    public void run() {
        ObjectInputStream in;
        try {
            in = new ObjectInputStream(clientSocket.getInputStream());

            Integer id;

            while(!clientSocket.isClosed()){
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
                    } else {
                        throw new UnknownPacketException("Fetcher got an unexpected packet.");
                    }
                } catch (ClassNotFoundException | UnknownPacketException e) {
                    e.printStackTrace();
                } catch (Exception e){
                    e.printStackTrace();
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
