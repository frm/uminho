package warehouse;

/**
 * Created by joaorodrigues on 15 Dec 14.
 */
public class AlreadySubscribedException extends WarehouseException{
    public AlreadySubscribedException() { super("AlreadySubscribingException"); }

    public AlreadySubscribedException(String msg) { super("AlreadySubscribingException", msg); }

}
