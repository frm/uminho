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

    /**
     * Constructor
     * @param name name of the item
     */
    public Item(String name) {
        this.name = name;
        this.quantity = 0;
        this.lock = new ReentrantLock();
        this.available = lock.newCondition();
    }

    /**
     * Constructor.
     * @param name name of the item
     * @param quantity initial quantity
     * @throws InvalidItemQuantityException
     */
    public Item(String name, int quantity) throws InvalidItemQuantityException {
        if (quantity < 0)
            throw new InvalidItemQuantityException("Quantity received: " + quantity + ". Must be > 0.");

        this.name = name;
        this.quantity = quantity;
        this.lock = new ReentrantLock();
        available = lock.newCondition();
    }

    // wakes up all threads waiting for this item
    private void signalAll(){
        this.available.signalAll();
    }

    /**
     * Wait until there is more quantity available
     * @throws InterruptedException
     */
    public void waitForMore() throws InterruptedException {
        this.lock();
        this.available.await();
        this.unlock();
    }

    /**
     * Lock the item
     */
    public void lock() {
        this.lock.lock();
    }

    /**
     * Unlock the item
     */
    public void unlock() {
        this.lock.unlock();
    }

    /**
     * Add the given quantity
     * @param quantity quantity to be added
     * @throws InvalidItemQuantityException
     */
    public void add(int quantity) throws InvalidItemQuantityException {
        this.lock();

        try {
            if(quantity < 0)
                throw new InvalidItemQuantityException("Quantity received: " + quantity + ". Must be > 0.");

            this.quantity += quantity;
            this.signalAll();
        } finally {
            this.unlock();
        }

    }

    /**
     * Remove the given quantity. Only works if there is enough quantity available
     * @param quantity Quantity to be removed
     * @throws InvalidItemQuantityException when the given quantity is smaller than the current quantity
     */
    public void remove(int quantity) throws InvalidItemQuantityException {
        this.lock();
        try {
            if(this.quantity < quantity)
                throw new InvalidItemQuantityException("Tried to remove: " + quantity + ". Has " + this.quantity);

            this.quantity -= quantity;
        } finally {
            this.unlock();
        }
    }

    /**
     *
     * @return current quantity
     */
    public int getQuantity() {
        this.lock();
        int qnt = this.quantity;
        this.unlock();
        return qnt;
    }

    /**
     * Checks if there is enough quantity
     * @param quantity quantity to check
     * @return
     */
    public boolean isAvailable(int quantity) {
        this.lock();
        boolean b = (quantity <= this.quantity);
        this.unlock();
        return b;
    }

    /**
     * Checks if the quantity available is > 0
     * @return
     */
    public boolean isAvailable() {
        return this.isAvailable(0);
    }
}
