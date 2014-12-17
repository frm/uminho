package warehouse;

import java.util.*;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Created by joaorodrigues on 14 Dec 14.
 */
public class TaskType {
    private static int idCount = 0;
    private int id;
    private String name;
    private Set<Integer> running;
    private Map<String, Integer> needs;
    private ReentrantLock needsLock;

    //Constructors

    public TaskType() {
        id = ++idCount;
        name = "";
        running = new HashSet<>();
        needs = new HashMap<>();
        needsLock = new ReentrantLock();
    }

    public TaskType(String na, Map<String, Integer> ne) {
        id = ++idCount;
        name = na;
        running = new HashSet<Integer>();
        needs = new HashMap<>(ne);
        needsLock = new ReentrantLock();
    }

    public synchronized void addTask(int id){
        needsLock.lock();
        running.add(id);
        needsLock.unlock();
    }


    //Getters & Setters

    public int getId() {
        return id;
    }

    public String getName() {
        return name;
    }


    public Map<String, Integer> getNeeds() {
        needsLock.lock();
        HashMap<String, Integer> result = new HashMap<String, Integer>(needs);
        needsLock.unlock();
        return result;
    }

    public void setId(int id) {
        this.id = id;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setNeeds(Map<String, Integer> ne) {
        needs = new HashMap<>(ne);
    }

    public Set<Integer> getRunning() {
        return new HashSet<Integer>(running);
    }

    public void setRunning(Set<Integer> running) {
        this.running = new HashSet<Integer>(running);
    }
}
