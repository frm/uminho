package warehouse;

/**
 * Created by tiago on 14-12-2014.
 */
public class InvalidItemQuantityException extends WarehouseException{
    public InvalidItemQuantityException() { super("InvalidItemQuantityException"); }

    public InvalidItemQuantityException(String msg) { super("InvalidItemQuantityException", msg); }

}
