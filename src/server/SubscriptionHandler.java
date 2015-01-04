package server;

import packet.Subscribe;
import warehouse.InexistentTaskException;
import warehouse.InexistentTaskTypeException;
import warehouse.Warehouse;

import java.io.IOException;
import java.util.Collection;

/**
 * Runnable class used to handle a subscription
 * Used to handle subscriptions asynchronously
 * Bridges Server and Warehouse
 */
public class SubscriptionHandler implements Runnable{
    private Sender sender;
    private Subscribe obj;
    private Warehouse warehouse;
    private Collection<Integer> ids;


    public SubscriptionHandler(Sender s, Subscribe o, Warehouse w){
        sender = s;
        obj = o;
        warehouse = w;
        ids = o.q_ids;
    }

    @Override
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
            sender.send(obj);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
