package warehouse;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Created by joaorodrigues on 17 Dec 14.
 */
public class Task {
    private static int nextId = 0;
    private final int id = ++nextId;
    private final int clientId;
    private Set<Integer> subscribers;
    private final String typeName;
    private ReentrantLock lock;


    public Task(int client, String type){
        clientId = client;
        subscribers = new HashSet<>();
        typeName = type;
        lock = new ReentrantLock();
    }


    public void notifySubscribers(){
            //TODO : tratar do user e das subs, depois voltar a isto
    }






    @Override
    public String toString(){
        StringBuilder result = new StringBuilder();

        lock.lock();
        result.append(typeName);
        result.append(" task (ID: ");
        result.append(id);
        result.append(" ran by client ");
        result.append(clientId);
        lock.unlock();
        
        return result.toString();
    }



    public void setSubscribers(Set<Integer> subscribers) {
        this.subscribers = subscribers;
    }

    public Set<Integer> getSubscribers() {
        return subscribers;
    }


    public int getId() {
        return id;
    }

    public int getClientId() {
        return clientId;
    }


    public String getTypeName() {
        return typeName;
    }

}
