package warehouse;

import java.util.concurrent.locks.ReentrantLock;

/**
 * Created by frmendes on 12/14/14.
 */
public class Item {
    String name;
    int quantity;
    ReentrantLock lock;

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

    public void lock() {
        this.lock();
    }

    public void unlock() {
        this.unlock();
    }

    public void add(int quantity) {
        this.quantity += quantity;
    }

    public void remove(int quantity) {
        this.quantity -= quantity;
    }

    public int getQuantity() {
        return this.quantity;
    }
}
