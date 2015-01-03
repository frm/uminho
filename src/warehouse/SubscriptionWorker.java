package warehouse;

/**
 * Created by joaorodrigues on 3 Jan 15.
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
