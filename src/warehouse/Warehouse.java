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

    public void newTaskType(String name, Map<String, Integer> items) throws ExistentTaskException {
        TaskType newTaskType = new TaskType(name, items);

        taskTypesLock.lock();
        try {
            if (taskTypes.containsKey(name)) {
                taskTypesLock.unlock();
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

        requestMaterial(type.getNeeds());
        taskTypesLock.lock();
        int taskId;
        try {
            type = taskTypes.get(typeName);

            if (type == null)
                throw new InexistentTaskTypeException("User referenced task type with name: " + typeName + " but was not found");

            type.lock();
            taskId = type.startTask(/*TODO: USER ID*/);
            type.unlock();
        }
        finally {
            taskTypesLock.unlock();
        }

        return taskId;
    }

    public void endTask(int id) throws InexistentTaskException, InexistentItemException {

        int typeId = TaskType.getTypeOfTask(id);

        taskTypesLock.lock();

        TaskType type = taskTypes.get(typeId);

        type.endTask(id);

        taskTypesLock.unlock();

        returnMaterial(type.getNeeds());
    }

    //Get list of tasks currently being done
    public String getRunningTasks() {
        StringBuilder result = new StringBuilder();

        taskTypesLock.lock();

        for( TaskType type: taskTypes.values()){
            result.append( type.getRunningString() );
        }
        taskTypesLock.unlock();

        return result.toString();

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
