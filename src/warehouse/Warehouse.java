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

    public int addTask(String name, Map<String, Integer> items) throws WarehouseException {
        tasksLock.lock();

        if( tasks.containsKey(name)) {
            tasksLock.unlock();
            throw new WarehouseException("TaskAlreadyExistsException");
        }

        tasksLock.unlock();
        Task newTask = new Task(name, items);
        int newTaskId = newTask.getId();
        
        tasksLock.lock();

        tasks.put(newTaskId, newTask);

        tasksLock.unlock();
        return newTaskId;

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
