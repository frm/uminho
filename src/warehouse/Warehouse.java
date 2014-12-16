package warehouse;

import java.util.*;
import java.util.concurrent.locks.ReentrantLock;

public class Warehouse {
    private Map<Integer, Task> tasks;
    private Map<String, Item> stock;
    private ReentrantLock stockLock;
    private ReentrantLock tasksLock;

    public Warehouse() {
        tasks = new HashMap<Integer, Task>();
        stock = new HashMap<String, Item>();
        stockLock = new ReentrantLock();
        tasksLock = new ReentrantLock();
    }

    // TODO: can that lock mixup cause a deadlock?
    public void stockUp(String itemName, int quantity) throws InvalidItemQuantityException {
        stockLock.lock();
        Item i = stock.get(itemName);
        stockLock.unlock();

        if(i == null)
            i = new Item(itemName);

        i.lock();
        i.add(quantity);

        stockLock.lock();
        stock.put(itemName, i);

        i.unlock();
        stockLock.unlock();
    }

    public int addTask(String name, Map<String, Integer> items) throws WarehouseException {
        tasksLock.lock();

        if( tasks.containsKey(name) ) {
            tasksLock.unlock();
            throw new TaskAlreadyExistsException();
        }

        tasksLock.unlock();
        Task newTask = new Task(name, items);
        int newTaskId = newTask.getId();

        tasksLock.lock();

        tasks.put(newTaskId, newTask);

        tasksLock.unlock();
        return newTaskId;
    }

    public void startTask(int id) throws InexistentTaskException, InexistentItemException {
        tasksLock.lock();
        Task t = tasks.get(id);
        tasksLock.unlock();

        if(t == null)
            throw new InexistentTaskException("User referenced task with id: " + id + " but was not found");

        requestMaterial(t.getNeeds());
        t.start();
    }

    public void endTask(int id) throws InexistentTaskException, InexistentItemException {
        tasksLock.lock();
        Task t = tasks.get(id);
        tasksLock.unlock();

        if(t == null)
            throw new InexistentTaskException("User referenced task with id: " + id + " but was not found");

        returnMaterial(t.getNeeds());
        t.stop();
    }

    //Get list of tasks currently being done
    public List<String> getRunningTasks() {
        ArrayList<String> result = new ArrayList<String>();
        tasksLock.lock();

        for (Task t : tasks.values()) {
            if (t.running())
                result.add(t.getName());
        }

        tasksLock.unlock();
        return result;
    }

    // TODO: remove should check if the value is bigger than the quantity and throw a new exception
    // TODO: Guiao 5, ex 2: nao esperar nos locks. eu tenho isto feito, e so juntar. Mendes
    private void requestMaterial(Map<String, Integer> material) throws InexistentItemException {
        stockLock.lock();
        for (Map.Entry<String, Integer> pair : material.entrySet()) {
            Item i = stock.get(pair.getKey());
            if(i == null)
                throw new InexistentItemException("User requested " + pair.getKey());

            i.lock();
            i.remove(pair.getValue());
            i.unlock();
        }
        stockLock.unlock();
    }

    private void returnMaterial(Map<String, Integer> material) throws InexistentItemException {
        stockLock.lock();

        try {
            for (Map.Entry<String, Integer> pair : material.entrySet()) {
                Item i = stock.get(pair.getKey());

                if (i == null)
                    throw new InexistentItemException("User returned " + pair.getKey());

                i.add(pair.getValue());
            }
        }finally {
            stockLock.unlock();
        }
    }
}
