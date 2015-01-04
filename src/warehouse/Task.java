package warehouse;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
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


    public Task(){
        running = true;
    }

    public Task(int userId) {
        this.running = true;
        this.userId = userId;
    }


    public void end(){
        lock.lock();
        running = false;
        subscription.signalAll();
        lock.unlock();
    }


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

    public int getUserId() {
        return this.userId;
    }

    public boolean belongsTo(int userId) {
        this.lock.lock();
        boolean b = this.userId == userId;
        this.lock.unlock();
        return b;
    }


    public int getId() {
        return id;
    }

}
