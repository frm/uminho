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

    /**
     *
     * @param na name of the task type
     * @param ne Map of article name/quantity that the task needs
     */
    public TaskType(String na, Map<String, Integer> ne) {
        name = na;
        running = new HashMap<>();
        needs = new HashMap<>(ne);
        runningLock = new ReentrantLock();
        mainLock = new ReentrantLock();
    }


    /**
     * Sets a task of this type, setting it to a owner
     * @param userId owner id
     * @return
     */
    public int startTask(int userId){
        Task t = new Task(userId);
        int taskId = t.getId();
        indexLock.lock();
        taskIndex.put(taskId, this.name);
        indexLock.unlock();

        runningLock.lock();
        running.put( taskId, t);
        runningLock.unlock();

        return taskId;
    }


    /**
     * Ends the task with the given id.
     * The userId is for to validate permissions.
     * @param taskId task id
     * @param userId id of the user that issued the end request
     * @throws UserNotAllowedException
     */
    public void endTask(int taskId, int userId ) throws UserNotAllowedException {
        runningLock.lock();
        Task t = running.get(taskId);

        if( !t.belongsTo(userId) ) {
            runningLock.unlock();
            throw new UserNotAllowedException("You don't have permissions to do that");
        }

        t.end();
        indexLock.lock();
        taskIndex.remove(taskId);
        indexLock.unlock();
        running.remove(taskId);
        runningLock.unlock();
    }

    /**
     * Returns the type of the task with the given id
     * @param taskId
     * @return
     * @throws InexistentTaskException
     */
    public static String getTypeOfTask(int taskId) throws InexistentTaskException {
        String type;
        indexLock.lock();
        try{
            type = taskIndex.get(taskId);

            if(type == null)
                throw new InexistentTaskException("User referenced task type with id: " + taskId + " but was not found");
        }
        finally{
            indexLock.unlock();
        }
        return type;
    }

    /**
     * Locks the object
     */
    public void lock(){
        mainLock.lock();
    }

    /**
     * unlocks the object
     */
    public void unlock(){
        mainLock.unlock();
    }

    /**
     * Returns the task with the given id
     * @param id
     * @return
     * @throws InexistentTaskException
     */
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

    /**
     *
     * @return name of this type
     */
    public String getName() {
        return name;
    }

    /**
     *
     * @return Map of item name/quantity that this type needs
     */
    public Map<String, Integer> getNeeds() {
        HashMap<String, Integer> result = new HashMap<String, Integer>(needs);
        return result;
    }

    /**
     * Updates the Map of Item name/quantity
     * @param ne
     */
    public void setNeeds(Map<String, Integer> ne) {
        needs = new HashMap<>(ne);
    }

    /**
     * Returns the collection of ids of the tasks that are currently active
     * @return
     */
    public Collection<Integer> getRunningIDs() {
        runningLock.lock();
        ArrayList<Integer> ids = new ArrayList<>( running.keySet() );
        runningLock.unlock();

        return ids;
    }
}
