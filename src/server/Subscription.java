package server;

import packet.Subscribe;
import warehouse.Task;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Created by joaorodrigues on 2 Jan 15.
 */
public class Subscription implements Runnable{
    private ObjectOutputStream stream;
    private Subscribe obj;
    private ArrayList<Task> list;

    public Subscription(ObjectOutputStream s, Subscribe o, ArrayList<Task> l ){
        stream = s;
        obj = o;
        list = l;
    }

    public void run(){
        int nrTasks = list.size();
        SubscriptionHelper helper = new SubscriptionHelper();

        for (Task t : list)
            ( new Thread( new SubscriptionWorker(t, helper) ) ).start();

        try {

            helper.waitForAll(nrTasks);
            stream.writeObject(obj);
            stream.flush();

        } catch (InterruptedException e) {
            obj.r_errors.add("Interrupted");
        } catch( IOException e){
            e.printStackTrace();
        }



    }

    //////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////////////

    public class SubscriptionWorker implements Runnable {
        private Task task;
        private SubscriptionHelper helper;

        public SubscriptionWorker(Task t, SubscriptionHelper h) {
            task = t;
            helper = h;
        }

        public void run() {
            try {
                task.subscribe();
                helper.finish();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }

    //////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////////////

    public class SubscriptionHelper {
        private int finished;
        private final ReentrantLock counterLock = new ReentrantLock();
        private final ReentrantLock lk = new ReentrantLock();
        private final Condition allDone = lk.newCondition();

        public SubscriptionHelper() {
            finished = 0;
        }

        public void finish() {
            counterLock.lock();
            finished++;
            counterLock.unlock();
            allDone.signalAll();
        }


        public void waitForAll(int nrTasks) throws InterruptedException {
            lk.lock();
            try {
                while (finished < nrTasks) {
                    allDone.await();
                }
            } finally {
                lk.unlock();
            }
        }

    }

}