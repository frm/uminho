package warehouse;

/**
 * Created by joaorodrigues on 15 Dec 14.
 */
public class TaskNotRunningException extends WarehouseException{
    public TaskNotRunningException() { super("NotBeingDoneException"); }

    public TaskNotRunningException(String msg) { super("NotBeingDoneException", msg); }

}
