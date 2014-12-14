package warehouse;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;

public class Warehouse {
    private Map<Integer, Task> tasks;
    private Map<String, Integer> stock;
    private ReentrantLock stockLock;
    private ReentrantLock tasksLock;

    public Warehouse() {
        tasks = new HashMap<Integer, Task>();
        stockLock = new ReentrantLock();
        tasksLock = new ReentrantLock();
    }

    public void stockUp(String item, int quantity) {
        // TODO
    }

    public int addTask(String name, Map<String, Integer> items) {
        // TODO
    }

    public void startTask(int id) throws InexistentTaskException {
        tasksLock.lock();
        Task t = tasks.get(id);
        tasksLock.unlock();

        if(t == null)
            throw new InexistentTaskException("User referenced task with id: " + id + " but was not found");

        t.start();
    }

    public void endTask(int id) {
        // TODO
    }
}
