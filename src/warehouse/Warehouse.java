package warehouse;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;

public class Warehouse {
    private Set<Task> tasks;
    private Map<String, Integer> stock;
    private ReentrantLock stockLock;
    private ReentrantLock tasksLock;

    public Warehouse() {
        tasks = new HashSet<Task>();
        stockLock = new ReentrantLock();
        tasksLock = new ReentrantLock();
    }

    public void stockUp(String item, int quantity) {
        // TODO
    }

    public int addTask(String name, Map<String, Integer> items) {
        // TODO
    }

    public void startTask(int id) {
        // TODO
    }

    public void endTask(int id) {
        // TODO
    }
}
