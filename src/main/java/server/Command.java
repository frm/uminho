package server;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by frm on 28/11/15.
 */
public class Command {
    public String command;
    public String[] args;

    public static final String REGISTER = "/create";
    private static final int REGISTER_ARGS = 2;
    public static final String AUTHENTICATE = "/auth";
    private static final int AUTHENTICATE_ARGS = 2;
    public static final String CANCEL = "/cancel";
    private static final int CANCEL_ARGS = 2;

    private static final HashMap<String, Integer> COMMAND_LIST = new HashMap<String, Integer>() {{
        put(REGISTER, REGISTER_ARGS);
        put(AUTHENTICATE, AUTHENTICATE_ARGS);
        put(CANCEL, CANCEL_ARGS);
    }};

    private Command(String c, String[] args) {
        command = c;
        this.args = args;
    }

    public boolean valid() {
        return COMMAND_LIST.containsKey(command) && correctNrArgs();
    }

    private boolean correctNrArgs() {
        for(Map.Entry<String, Integer> p : COMMAND_LIST.entrySet()) {
            if(p.getKey().equals(command))
                return p.getValue() == args.length;
        }

        return false;
    }

    public static Command parse(String req) {
        String[] strs = req.trim().split(" ");
        Command c = new Command(strs[0], Arrays.copyOfRange(strs, 1, strs.length));
        return c.valid() ? c : new Command("", new String[0]);
    }
}
