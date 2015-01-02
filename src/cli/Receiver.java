package cli;


import java.io.Serializable;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

public class Receiver<T extends Serializable>{
    private T obj;
    Boolean hasValue;
    ReentrantLock lock;
    Condition valueSet;

    private static Integer nextID = 0;
    private static ReentrantLock idLock = new ReentrantLock();

    private static Integer getNextID(){
        Integer resultado;

        idLock.lock();
        resultado = nextID++;
        idLock.unlock();

        return resultado;
    }

    Receiver(Fetcher fetcher){
        hasValue = false;
        lock = new ReentrantLock();
        valueSet = lock.newCondition();

        fetcher.addReceiver(getNextID(), this);
    }

    public T get(){
        T value;

        lock.lock();

        while (!hasValue) {
            try {
                valueSet.await();
            } catch (InterruptedException e) {}
        }
        value = obj;

        lock.unlock();

        return value;
    }

    public void set(T value){
        lock.lock();

        obj = value;

        lock.unlock();

        valueSet.signalAll();
    }
}
