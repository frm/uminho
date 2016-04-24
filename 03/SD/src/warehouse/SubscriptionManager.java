package warehouse;


import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Class responsible for managing SubscriptionWorker objects.
 */

public class SubscriptionManager {
    private int finished;
    private final ReentrantLock lk = new ReentrantLock();
    private final Condition allDone = lk.newCondition();

    public SubscriptionManager() {
        finished = 0;
    }

    /**
     * Increase the number of finished Tasks
     */
    public synchronized void finish() {
        finished++;
        lk.lock();
        allDone.signalAll();
        lk.unlock();
    }

    /**
     * Waits for all Tasks to finish
     * Uses a condition variable, signalled everytime a task is finished
     */
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