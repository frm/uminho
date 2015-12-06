package communication;

import util.Pair;

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
    public static final String JOIN = "/join";
    private static final int JOIN_ARGS = 1;
    public static final String PM = "/msg";
    private static final int PM_ARGS = 1;
    public static final String LIST_ROOMS = "/list";
    private static final int LIST_ROOMS_ARGS = 0;

    private static final HashMap<String, Pair<Integer, Boolean>> COMMAND_LIST =
        new HashMap<String, Pair<Integer, Boolean>>() {{
            // second argument of pair indicates the strictness
            // false states that the number of arguments is strict
            // true states that the number of arguments is the minimum required
            put(REGISTER, new Pair<>(REGISTER_ARGS, false));
            put(AUTHENTICATE, new Pair<>(AUTHENTICATE_ARGS, false));
            put(CANCEL, new Pair<>(CANCEL_ARGS, false));
            put(JOIN, new Pair<>(JOIN_ARGS, false));
            put(LIST_ROOMS, new Pair<>(LIST_ROOMS_ARGS, false));
            put(PM, new Pair<>(PM_ARGS, true));
    }};

    private Command(String c, String[] args) {
        command = c;
        this.args = args;
    }

    public boolean valid() {
        return COMMAND_LIST.containsKey(command) && correctNrArgs();
    }

    private boolean correctNrArgs() {
        for(Map.Entry<String, Pair<Integer, Boolean>> p : COMMAND_LIST.entrySet()) {
            if(p.getKey().equals(command))
                if(p.getValue().second)
                    return p.getValue().first >= args.length;
                else
                    return p.getValue().first == args.length;
        }

        return false;
    }

    public static Command parse(String req) {
        String[] strs = req.trim().split(" ");
        Command c = new Command(strs[0], Arrays.copyOfRange(strs, 1, strs.length));
        return c.valid() ? c : new Command("", new String[0]);
    }
}
