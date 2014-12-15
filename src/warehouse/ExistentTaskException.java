package warehouse;

/**
 * Created by joaorodrigues on 14 Dec 14.
 */
public class ExistentTaskException extends WarehouseException {
    public ExistentTaskException() {
        super("TaskAlreadyExistsException");
    }

    public ExistentTaskException(String msg) {
        super("TaskAlreadyExistsException", msg);
    }
}