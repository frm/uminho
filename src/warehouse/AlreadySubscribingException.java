package warehouse;

/**
 * Created by joaorodrigues on 15 Dec 14.
 */
public class AlreadySubscribingException extends WarehouseException{
    public AlreadySubscribingException() { super("AlreadySubscribingException"); }

    public AlreadySubscribingException(String msg) { super("AlreadySubscribingException", msg); }

}
