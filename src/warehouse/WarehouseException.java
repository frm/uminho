package warehouse;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by frmendes on 12/14/14.
 */
public class WarehouseException extends Exception {
    private static Map<String, String> EXCEPTIONS = new HashMap<String, String>() {{
        this.put("InexistentTaskException", "You referenced a task that does not exist.");
        this.put("InexistentTaskTypeException", "You referenced a task type that does not exist.");
        this.put("ExistentTaskException", "The task you tried to add already exists");
        this.put("TaskNotRunningException", "The task can't be stopped because it's not being done by any client");
        this.put("NotSubscribedException", "That ID is not subscribed to that task");
        this.put("AlreadySubscribedException", "That ID is already subscribed to that task");
        this.put("UnknownPacketException", "Received an unexpected packet");
        this.put("UserNotAllowedException", "You are not allowed to do that");
    }};

    private String className;

    public WarehouseException() {
        super();
    }

    public WarehouseException(String className) {
        super(EXCEPTIONS.get(className));
        this.className = className;
    }

    public WarehouseException(String className, String message) {
        super(EXCEPTIONS.get(className));
        this.className = className;
    }

    public String getUserMessage(){
        return "[" + className + "] " + EXCEPTIONS.get(className);
    }
}
