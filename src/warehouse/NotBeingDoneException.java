package warehouse;

/**
 * Created by joaorodrigues on 15 Dec 14.
 */
public class NotBeingDoneException extends WarehouseException{
    public NotBeingDoneException() { super("NotBeingDoneException"); }

    public NotBeingDoneException(String msg) { super("NotBeingDoneException", msg); }

}
