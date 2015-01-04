package warehouse;

/**
 * Created by frmendes on 1/4/15.
 */
public class UserNotAllowedException extends WarehouseException {
    public UserNotAllowedException() { super("UserNotAllowedException"); }

    public UserNotAllowedException(String msg) { super("UserNotAllowedException", msg); }
}
