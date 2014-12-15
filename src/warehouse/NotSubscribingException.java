package warehouse;

/**
 * Created by joaorodrigues on 15 Dec 14.
 */
public class NotSubscribingException extends WarehouseException{
    public NotSubscribingException() { super("NotSubscribingException"); }

    public NotSubscribingException(String msg) { super("NotSubscribingException", msg); }

}