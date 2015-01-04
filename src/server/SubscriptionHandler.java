package server;

import packet.Subscribe;
import warehouse.InexistentTaskException;
import warehouse.InexistentTaskTypeException;
import warehouse.Warehouse;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Collection;

/**
 * Created by joaorodrigues on 3 Jan 15.
 */
public class SubscriptionHandler implements Runnable{
    private ObjectOutputStream stream;
    private Subscribe obj;
    private Warehouse warehouse;
    private Collection<Integer> ids;


    public SubscriptionHandler(ObjectOutputStream s, Subscribe o, Warehouse w){
        stream = s;
        obj = o;
        warehouse = w;
        ids = o.q_ids;
    }

    public void run(){
        try {
            warehouse.subscribeTo(ids);
        } catch (InexistentTaskTypeException e) {
            obj.r_errors.add(e.getUserMessage());
        } catch (InexistentTaskException e) {
            obj.r_errors.add(e.getUserMessage());
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        try{
            stream.writeObject(obj);
            stream.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
