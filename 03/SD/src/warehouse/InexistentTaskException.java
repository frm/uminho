package warehouse;

/**
 * Created by frmendes on 12/14/14.
 */
public class InexistentTaskException extends WarehouseException {
    public InexistentTaskException() {
        super("InexistentTaskException");
    }

    public InexistentTaskException(String msg) {
        super("InexistentTaskException", msg);
    }
}
