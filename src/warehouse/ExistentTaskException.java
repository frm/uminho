package warehouse;

/**
 * Created by joaorodrigues on 14 Dec 14.
 */
public class ExistentTaskException extends WarehouseException {
    public ExistentTaskException() {
        super("ExistentTaskException");
    }

    public ExistentTaskException(String msg) {
        super("ExistentTaskException", msg);
    }
}
