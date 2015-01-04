package warehouse;

/**
 * Runnable class used to wait for the end of one Task ( using Task#subscribe ) and alert its manager once that happens.
 */
public class SubscriptionWorker implements Runnable {
    private Task task;
    private SubscriptionManager manager;

    public SubscriptionWorker(Task t, SubscriptionManager h) {
        task = t;
        manager = h;
    }

    public void run() {
        try {
            task.subscribe();
            manager.finish();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
