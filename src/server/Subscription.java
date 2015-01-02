package server;

import warehouse.InexistentTaskException;
import warehouse.Task;
import warehouse.Warehouse;

import java.util.ArrayList;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Created by joaorodrigues on 2 Jan 15.
 */
public class Subscription {
    private Warehouse warehouse;
    private TaskCounter counter;
    private final ReentrantLock lk = new ReentrantLock();
    private final Condition allDone = lk.newCondition();


    //////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////////////

    public class TaskCounter {
        private int finished;
        private final ReentrantLock counterLock = new ReentrantLock();

        public TaskCounter() {
            finished = 0;
        }

        public void increment() {
            counterLock.lock();
            finished++;
            counterLock.unlock();
        }

        public void decrement() {
            counterLock.lock();
            finished--;
            counterLock.unlock();
        }

        public int getValue() {
            int result;
            counterLock.lock();
            result = finished;
            counterLock.unlock();
            return result;
        }
    }

    //////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////////////

    public class SubscriptionWorker implements Runnable {
        private Task task;
        private TaskCounter counter;
        private Condition allDone;

        public SubscriptionWorker(Task t, TaskCounter c, Condition cond) {
            task = t;
            this.counter = c;
            this.allDone = cond;
        }

        public void run() {
            try {
                task.subscribe();
                this.counter.increment();
                allDone.signalAll();
            } catch (InterruptedException e) {
                System.err.println("Interrupted");
            }
        }
    }

    //////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////////////

    public Subscription(Warehouse w) {
        warehouse = w;
        counter = new TaskCounter();
    }

    public void subscribeTo(ArrayList<Task> list) throws InexistentTaskException {
        int nrTasks = list.size();
        for (Task t : list) {
            (new Thread(new SubscriptionWorker(t, counter, allDone))).start();
        }

        lk.lock();
        try {
            while (counter.getValue() < nrTasks) {
                allDone.await();
            }
        } catch (InterruptedException e) {
            System.err.println("Interrupted");
        } finally {
            lk.unlock();
        }
    }
}