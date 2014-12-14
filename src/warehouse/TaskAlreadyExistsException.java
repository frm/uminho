package warehouse;

/**
 * Created by joaorodrigues on 14 Dec 14.
 */
public class TaskAlreadyExistsException extends WarehouseException {
    public TaskAlreadyExistsException() {
        super("TaskAlreadyExistsException");
    }

    public TaskAlreadyExistsException(String msg) {
        super("TaskAlreadyExistsException", msg);
    }
}