package warehouse;


import java.util.*;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;
/**
 * Class used to manage stock, tasks and subscriptions
 */
public class Warehouse {
    private Map<String, TaskType> taskTypes;
    private Map<String, Item> stock;
    private ReentrantLock stockLock;
    private ReentrantLock taskTypesLock;
    Condition turn;
    PriorityQueue<Long> removeQueue;
    long nextTicket;

    /**
     * Static variable that limits the number of times the current user can be overtaken (concurrency control)
     */
    private static final int REM_QUEUE_LIMIT = 4;

    /**
     * Empty Constructor
     * All data structures start empty
     */
    public Warehouse() {
        taskTypes = new HashMap<>();
        stock = new HashMap<>();
        stockLock = new ReentrantLock();
        taskTypesLock = new ReentrantLock();
        this.turn = stockLock.newCondition();
        this.removeQueue = new PriorityQueue<>();
        this.nextTicket = 1;
    }

    /**
     * Increase the quantity of an item. If the item does not exist, create it.
     */
    public void stockUp(String itemName, int quantity) throws InvalidItemQuantityException {
        stockLock.lock();
        Item i = stock.get(itemName);

        if(i == null)
            i = new Item(itemName);

        i.lock();
        stockLock.unlock();

        try {
            i.add(quantity);
        } finally {
            i.unlock();
        }

        stockLock.lock();
        stock.put(itemName, i);

        stockLock.unlock();
    }

    /**
     * Creates a new task type
     * @param name name of the task type
     * @param needs item quantities required to start the task
     */
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

    /**
     * Starts a task
     * Removes the material from the stock and adds the task to the "taskIndex" and "tasks"
     * @return The created task's ID
     */
    public int startTask(String typeName, int userId) throws InexistentTaskTypeException, InexistentItemException, InterruptedException {
        TaskType type;

        taskTypesLock.lock();
        int taskId;
        try {
            type = taskTypes.get(typeName);

            if (type == null)
                throw new InexistentTaskTypeException("User referenced task type with name: " + typeName + " but was not found");

            taskTypesLock.unlock();
            Map<String, Integer> needs = type.getNeeds();
            requestMaterial(needs);
            taskTypesLock.lock();

            type.lock();
            taskId = type.startTask(userId);
            type.unlock();
        }
        finally {
            taskTypesLock.unlock();
        }

        return taskId;
    }

    /**
     * Finished a task
     * Returns the material to the stock and removes the task from "taskIndex" and "tasks"
     */
    public void endTask(int id, int userId) throws InexistentTaskException, InexistentItemException, UserNotAllowedException {
        String typeName = TaskType.getTypeOfTask(id);

        taskTypesLock.lock();

        TaskType type = taskTypes.get(typeName);

        type.lock();
        taskTypesLock.unlock();

        Map<String, Integer> needs = null;
        try {
            type.endTask(id, userId);
            needs = type.getNeeds();
        } finally {
            type.unlock();
        }

        try {
                returnMaterial(needs);
        } catch (InvalidItemQuantityException e) {} // Since we are returning a value that is already valid, the exception never occurs

    }

    /**
     * Gathers the task types and their instances
     * @return A Map with the type name (String) and the ids of its instances (Collection<Integer>)
     */
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

    /**
     * Gets the task with ID "id"
     * @return Task with ID == "id"
     */
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

    /**
     * Subscribes the current user to the tasks with the given IDs
     * Starts a thread for each Task to wait for its termination
     * The main thread waits until all tasks are done
     */
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


    /**
     * Removes the given quantities of the given items from the stock
     * @param material Map with the item names (String) and the quantity (Integer) to remove from that item in the stock
     */
    private void requestMaterial(Map<String, Integer> material) throws InexistentItemException, InterruptedException {
        stockLock.lock();

        long myTurn = nextTicket++;
        Iterator<Map.Entry<String, Integer>> it = material.entrySet().iterator();

        while( it.hasNext() ) {
            Map.Entry<String, Integer> pair = it.next();
            Item i = stock.get(pair.getKey());
            boolean waited = false;

            while( !i.isAvailable( pair.getValue() ) ) {
                stockLock.unlock();
                i.waitForMore();
                stockLock.lock();
                waited = true;
                it = material.entrySet().iterator();// rewinding the iterator
            }

            if(waited)
                continue;

            Long next = removeQueue.peek();

            while(next != null && myTurn > next.longValue() + REM_QUEUE_LIMIT) { // peek returns null if the queue is empty
                removeQueue.add(myTurn);
                this.turn.await();
                removeQueue.remove(myTurn);
                next = removeQueue.peek();
                it = material.entrySet().iterator();        // rewinding the iterator
            }

        }

        for(Map.Entry<String, Integer> pair : material.entrySet()) {
            try {
                stock.get( pair.getKey() ).remove( pair.getValue() );
            } catch (InvalidItemQuantityException e) {} // never occurs because the task validates its needs upon creation. we always remove valid values
        }

        this.turn.signalAll();
        stockLock.unlock();
    }

    /**
     * Returns the given quantities of the given items to the stock
     * @param material Map with the item names (String) and the quantity (Integer) to return to that item in the stock
     */
    private void returnMaterial(Map<String, Integer> material) throws InexistentItemException, InvalidItemQuantityException {

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
