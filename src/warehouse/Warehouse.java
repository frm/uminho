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

    private void requestMaterial(Map<String, Integer> material) throws InexistentItemException {
        while(true) {
            stockLock.lock();

            Map<Item, Integer> requests = new HashMap<>(); // to unlock stock earlier

            // lock the itens
            try {
                for (Map.Entry<String, Integer> pair : material.entrySet()) {
                    Item i = stock.get(pair.getKey());

                    if (i == null)
                        throw new InexistentItemException("User requested " + pair.getKey());

                    i.lock();
                    requests.put(i, pair.getValue()); //save name and item
                }
            } catch (Exception e) {
                //unlock everything because there was an InexistentItemException
                for (Map.Entry<Item, Integer> pair : requests.entrySet())
                    pair.getKey().unlock();

                throw e; // deal with the exception somewhere else
            } finally {
                stockLock.unlock();
            }

            // check quantities
            Item unavailable = null;
            for (Map.Entry<Item, Integer> pair : requests.entrySet()) {
                if (pair.getKey().getQuantity() - pair.getValue() < 0) {
                    // out of stock!
                    unavailable = pair.getKey();
                    break;
                }
            }

            // if there was an unavailable item, start all over
            if(unavailable != null){
                requests.remove(unavailable); //save it for last

                // unlock everything else
                for (Map.Entry<Item, Integer> pair : requests.entrySet())
                    pair.getKey().unlock();

                // wait for it
                try {
                    unavailable.await(); //when it awakes, it will try the whole thing again
                } catch (InterruptedException e) {
                    // nevermind this one, proceed
                }

                continue; // start all over.
            }

            // everything we need is available, remove quantities
            for (Map.Entry<Item, Integer> pair : requests.entrySet()) {
                pair.getKey().remove(pair.getValue());
                pair.getKey().unlock();
            }

            // getting here means the needed quantities have been removed.
            break;
        }
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
