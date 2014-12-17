package warehouse;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Created by joaorodrigues on 17 Dec 14.
 */
public class Task {
    private static int idCount = 0;
    private int id;
    private int clientId;
    private Set<Integer> subscribers;
    private String typeName;
    private ReentrantLock lock;


    public Task(int client, String type){
        id = ++idCount;
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

    public static void setIdCount(int idCount) {
        Task.idCount = idCount;
    }

    public void setId(int id) {
        this.id = id;
    }

    public void setClientId(int clientId) {
        this.clientId = clientId;
    }

    public static int getIdCount() {
        return idCount;
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

    public void setTypeName(String typeName) {
        this.typeName = typeName;
    }
}
