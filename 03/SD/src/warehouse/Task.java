package warehouse;

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Created by joaorodrigues on 17 Dec 14.
 */
public class Task {
    private static int nextId = 0;
    private final int id = ++nextId;
    private ReentrantLock lock = new ReentrantLock();
    private boolean running;
    private final Condition subscription = lock.newCondition();
    private int userId;


    /**
     * Constructor
     * @param userId id of the owner
     */
    public Task(int userId) {
        this.running = true;
        this.userId = userId;
    }

    /**
     * Terminate a task.
     * Signals all subscribers that it has ended.
     */
    public void end() {
        lock.lock();
        running = false;
        subscription.signalAll();
        lock.unlock();
    }


    /**
     * Sets the current thread to await until the it is ends.
     * @throws InterruptedException
     */
    public void subscribe() throws InterruptedException {
        lock.lock();
        try {
            while (running) {
                subscription.await();
            }
        }
        finally{
            lock.unlock();
        }
    }

    /**
     *
     * @return owner id
     */
    public int getUserId() {
        return this.userId;
    }

    /**
     * checks if a task belongs to a user
     * @param userId
     * @return
     */
    public boolean belongsTo(int userId) {
        this.lock.lock();
        boolean b = this.userId == userId;
        this.lock.unlock();
        return b;
    }


    /**
     *
     * @return task id
     */
    public int getId() {
        return id;
    }

}
