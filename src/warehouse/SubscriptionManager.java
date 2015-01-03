package warehouse;

import packet.Subscribe;
import warehouse.Task;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Created by joaorodrigues on 2 Jan 15.
 */

public class SubscriptionManager {
    private int finished;
<<<<<<< HEAD
=======
    private final ReentrantLock counterLock = new ReentrantLock();
>>>>>>> Improved the subscriptions and newTaskType
    private final ReentrantLock lk = new ReentrantLock();
    private final Condition allDone = lk.newCondition();

    public SubscriptionManager() {
        finished = 0;
    }

<<<<<<< HEAD
    public synchronized void finish() {
        finished++;
=======
    public void finish() {
        counterLock.lock();
        finished++;
        counterLock.unlock();
>>>>>>> Improved the subscriptions and newTaskType
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