package warehouse;

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Created by frmendes on 12/14/14.
 */
public class Item {
    String name;
    int quantity;
    ReentrantLock lock;
    Condition available; //wake up when not empty

    public Item(String name) {
        this.name = name;
        this.quantity = 0;
        this.lock = new ReentrantLock();
    }

    public Item(String name, int quantity) throws InvalidItemQuantityException {
        if (quantity <= 0)
            throw new InvalidItemQuantityException("Quantity received: " + quantity + ". Must be > 0.");

        this.name = name;
        this.quantity = quantity;
        this.lock = new ReentrantLock();
    }

    // wakes up all threads waiting for this item
    public void signalAll(){
        this.available.signalAll();
    }

    // go to sleep until we have this item
    public void waitForMore() throws InterruptedException {
        this.available.await();
    }

    public void lock() {
        this.lock.lock();
    }

    public void unlock() {
        this.lock.unlock();
    }

    public void add(int quantity) throws InvalidItemQuantityException {
        this.lock();

        try {
            if(quantity <= 0)
                throw new InvalidItemQuantityException("Quantity received: " + quantity + ". Must be > 0.");

            this.quantity += quantity;
            this.signalAll();
        } finally {
            this.unlock();
        }

    }

    public void remove(int quantity) throws InvalidItemQuantityException {
        this.lock();
        try {
            if(this.quantity < quantity)
                throw new InvalidItemQuantityException("Quantity received: " + quantity + ". Must be > " + this.quantity);

            this.quantity -= quantity;
        } finally {
            this.unlock();
        }
    }

    public int getQuantity() {
        this.lock();
        int qnt = this.quantity;
        this.unlock();
        return qnt;
    }

    public boolean isAvailable(int quantity) {
        this.lock();
        boolean b = quantity >= this.quantity;
        this.unlock();
        return b;
    }

    public boolean isAvailable() {
        return this.isAvailable(0);
    }
}
