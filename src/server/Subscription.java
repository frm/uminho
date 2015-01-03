package server;

import warehouse.Task;
import warehouse.Warehouse;

import java.util.ArrayList;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Created by joaorodrigues on 2 Jan 15.
 */
public class Subscription {


    public void subscribeTo(ArrayList<Task> list) throws InterruptedException {
        int nrTasks = list.size();
        SubscriptionHelper helper = new SubscriptionHelper();

        for (Task t : list) {
            ( new Thread( new SubscriptionWorker(t, helper) ) ).start();
        }

        helper.waitForDone(nrTasks);

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
                System.err.println("Interrupted");
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


        public void waitForDone(int nrTasks) throws InterruptedException {
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