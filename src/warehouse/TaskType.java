package warehouse;

import java.util.*;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Created by joaorodrigues on 14 Dec 14.
 */
public class TaskType {
    private static HashMap<Integer, String> taskIndex = new HashMap<>();
    private static ReentrantLock indexLock = new ReentrantLock();

    private final String name;

    private Map<Integer, Task> running;
    private Map<String, Integer> needs;

    private ReentrantLock runningLock;
    private ReentrantLock mainLock;

    //Constructors

    public TaskType(String na, Map<String, Integer> ne) {
        name = na;
        running = new HashMap<>();
        needs = new HashMap<>(ne);
        runningLock = new ReentrantLock();
        mainLock = new ReentrantLock();
    }


    public int startTask(){
        Task t = new Task();
        int taskId = t.getId();
        indexLock.lock();
        taskIndex.put(taskId, this.name);
        indexLock.unlock();

        runningLock.lock();
        running.put( taskId, t);
        runningLock.unlock();

        return taskId;
    }


    public void endTask( int taskId ){
        runningLock.lock();
        ( running.get(taskId) ).end();
        running.remove(taskId);
        runningLock.unlock();
    }

    public static String getTypeOfTask(int taskId) throws InexistentTaskTypeException {
        String type;
        indexLock.lock();
        try{
            type = taskIndex.get(taskId);

            if(type == null)
                throw new InexistentTaskTypeException("User referenced task type with name: " + type + " but was not found");
        }
        finally{
            indexLock.unlock();
        }
        return type;
    }

    public void lock(){
        mainLock.lock();
    }

    public void unlock(){
        mainLock.unlock();
    }

    public Task getTask(int id) throws InexistentTaskException {
        Task result;

        runningLock.lock();
        result = running.get(id);
        runningLock.unlock();
        if(result == null)
            throw new InexistentTaskException("User referenced task with id: " + id + " but was not found");

        return result;
    }

    //Getters & Setters

    public String getName() {
        return name;
    }

    public Map<String, Integer> getNeeds() {
        HashMap<String, Integer> result = new HashMap<String, Integer>(needs);
        return result;
    }

    public void setNeeds(Map<String, Integer> ne) {
        needs = new HashMap<>(ne);
    }

    public Collection<Integer> getRunningIDs() {
        runningLock.lock();
        ArrayList<Integer> ids = new ArrayList<>( running.keySet() );
        runningLock.unlock();

        return ids;
    }

    public void setRunning(Map<Integer, Task> running) {
        this.running = new HashMap<Integer, Task>(running);
    }

}
