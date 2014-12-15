package warehouse;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Created by joaorodrigues on 14 Dec 14.
 */
public class Task {
    private static int idCount = 0;
    private int id;
    private String name;
    private int doing;
    private Set<Integer> subscribers;
    private Map<String, Integer> needs;
    private ReentrantLock taskLock;

    //Constructors

    public Task() {
        id = ++idCount;
        name = "";
        doing = 0;
        subscribers = new HashSet<>();
        needs = new HashMap<>();
        taskLock = new ReentrantLock();
    }

    public Task(String na, Map<String, Integer> ne) {
        id = ++idCount;
        name = na;
        doing = 0;
        subscribers = new HashSet<>();
        needs = new HashMap<>(ne);
        taskLock = new ReentrantLock();
    }

    //Subscribe and unsubscribe

    public void addSubscriber(int i){
        taskLock.lock();
        subscribers.add(i);
        taskLock.unlock();
    }

    public void removeSubscriber(int i){
        taskLock.lock();
        subscribers.remove(i);
        taskLock.unlock();
    }

    //start and stop, for when a client starts or stops doing a task

    public void start(){
        taskLock.lock();
        doing++;
        taskLock.unlock();
    }

    public void stop(){
        taskLock.lock();
        doing--;
        taskLock.unlock();
    }

    public boolean running(){
        taskLock.lock();
        boolean result = doing > 0;
        taskLock.unlock();
        return result;
    }



    //Getters & Setters

    public int getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public int getDoing() {
        return doing;
    }

    public Set<Integer> getSubscribers() {
        return new HashSet<Integer>(subscribers);
    }

    public Map<String, Integer> getNeeds() {
        return new HashMap<String, Integer>(needs);
    }

    public void setId(int id) {
        this.id = id;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setDoing(int doing) {
        this.doing = doing;
    }

    public void setSubscribers(Set<Integer> subs) {
        subscribers = new HashSet<Integer>(subs);
    }

    public void setNeeds(Map<String, Integer> ne) {
        needs = new HashMap<>(ne);
    }
}
