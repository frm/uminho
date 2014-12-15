package warehouse;

/**
 * Created by frmendes on 12/14/14.
 */
public class InexistentItemException extends WarehouseException {
    public InexistentItemException() {
        super("InexistentItemException");
    }

    public InexistentItemException(String msg) {
        super("InexistentItemException", msg);
    }
}
