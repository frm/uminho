package warehouse;

/**
 * Created by joaorodrigues on 15 Dec 14.
 */
public class NotSubscribedException extends WarehouseException{
    public NotSubscribedException() { super("NotSubscribedException"); }

    public NotSubscribedException(String msg) { super("NotSubscribedException", msg); }

}