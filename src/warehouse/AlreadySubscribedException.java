package warehouse;

/**
 * Created by joaorodrigues on 15 Dec 14.
 */
public class AlreadySubscribedException extends WarehouseException{
    public AlreadySubscribedException() { super("AlreadySubscribedException"); }

    public AlreadySubscribedException(String msg) { super("AlreadySubscribedException", msg); }

}
