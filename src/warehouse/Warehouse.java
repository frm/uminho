package warehouse;

import java.util.*;
import java.util.concurrent.locks.ReentrantLock;

public class Warehouse {
    private Map<String, TaskType> taskTypes;
    private Map<String, Item> stock;
    private ReentrantLock stockLock;
    private ReentrantLock taskTypesLock;

    public Warehouse() {
        taskTypes = new HashMap<>();
        stock = new HashMap<>();
        stockLock = new ReentrantLock();
        taskTypesLock = new ReentrantLock();
    }

    // TODO: can that lock mixup cause a deadlock?
    public void stockUp(String itemName, int quantity) throws InvalidItemQuantityException {
        stockLock.lock();
        Item i = stock.get(itemName);


        if(i == null)
            i = new Item(itemName);

        i.lock();

        stockLock.unlock();

        i.add(quantity);

        stockLock.lock();
        stock.put(itemName, i);

        i.unlock();
        stockLock.unlock();
    }

    public void newTaskType(String name, Map<String, Integer> needs) throws ExistentTaskException, InvalidItemQuantityException {
        for(Map.Entry<String, Integer> pair: needs.entrySet()){

            if(pair.getValue() <= 0)
                throw new InvalidItemQuantityException();

            String itemName = pair.getKey();

            stockLock.lock();

            if( stock.get(itemName) == null )
                stock.put(itemName, new Item(itemName));

            stockLock.unlock();
        }

        TaskType newTaskType = new TaskType(name, needs);

        taskTypesLock.lock();
        try {
            if (taskTypes.containsKey(name)) {
                throw new ExistentTaskException();
            }

            taskTypes.put(name, newTaskType);
        }
        finally {
            taskTypesLock.unlock();
        }

    }

    public int startTask(String typeName) throws InexistentTaskTypeException, InexistentItemException {
        TaskType type;

        //requestMaterial(type.getNeeds());
        taskTypesLock.lock();
        int taskId;
        try {
            type = taskTypes.get(typeName);

            if (type == null)
                throw new InexistentTaskTypeException("User referenced task type with name: " + typeName + " but was not found");

            type.lock();
            taskId = type.startTask();
            type.unlock();
        }
        finally {
            taskTypesLock.unlock();
        }

        return taskId;
    }

    public void endTask(int id) throws InexistentTaskTypeException, InexistentItemException {

        String typeName = TaskType.getTypeOfTask(id);

        taskTypesLock.lock();

        TaskType type = taskTypes.get(typeName);

        type.lock();
        taskTypesLock.unlock();

        type.endTask(id);
        Map<String, Integer> needs = type.getNeeds();

        type.unlock();

        returnMaterial(needs);
    }

    //Get list of tasks currently being done
    public Map<String, Collection<Integer>> getRunningTasks() {

        Map<String, Collection<Integer>> result = new HashMap<>();

        taskTypesLock.lock();

        for( TaskType type : taskTypes.values()) {
            type.lock();
            result.put(type.getName(), type.getRunningIDs());
            type.unlock();
        }

        taskTypesLock.unlock();

        return result;

    }

    public Task getTask(int id) throws InexistentTaskTypeException, InexistentTaskException {
        String typeName = TaskType.getTypeOfTask(id);

        taskTypesLock.lock();
        TaskType type = taskTypes.get(typeName);

        type.lock();
        taskTypesLock.unlock();

        Task result = type.getTask(id);

        type.unlock();
        return result;
    }

    public void subscribeTo(Collection<Integer> ids) throws InexistentTaskTypeException, InexistentTaskException, InterruptedException {
        ArrayList<Task> tasks = new ArrayList<>();
        int nrTasks = ids.size();

        for(int id: ids) {
            tasks.add(getTask(id));
        }

        SubscriptionManager manager = new SubscriptionManager();

        for (Task t : tasks)
            ( new Thread( new SubscriptionWorker(t, manager) ) ).start();

        manager.waitForAll(nrTasks);

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

    private void returnMaterial(Map<String, Integer> material){
        stockLock.lock();

            for (Map.Entry<String, Integer> pair : material.entrySet()) {
                Item i = stock.get(pair.getKey());

                i.lock();
                stockLock.unlock();

                i.add(pair.getValue());

                i.unlock();
                stockLock.lock();


            }

        stockLock.unlock();
    }
}
