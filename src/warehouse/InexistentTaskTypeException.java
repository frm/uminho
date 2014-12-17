package warehouse;

/**
 * Created by joaorodrigues on 17 Dec 14.
 */
public class InexistentTaskTypeException extends WarehouseException {
    public InexistentTaskTypeException() {
        super("InexistentTaskTypeException");
    }

    public InexistentTaskTypeException(String msg) {
        super("InexistentTaskTypeException", msg);
    }
}
