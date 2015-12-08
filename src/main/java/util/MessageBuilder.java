package util;

import co.paralleluniverse.fibers.Suspendable;
import org.yaml.snakeyaml.Yaml;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.Map;

/**
 * Created by frm on 30/11/15.
 */
public class MessageBuilder {
    private static final String MESSAGE_FILE = "./src/main/java/util/message.yml";
    private static Map<String, String> messages;
    private static Yaml file;

    public static String INVALID_PARAMS="invalid_params";
    public static String INVALID_COMMAND="invalid_command";
    public static String AUTH_SUCCESS="auth_success";
    public static String AUTH_INVALID="auth_invalid";
    public static String NOT_AUTHENTICATED="not_authenticated";
    public static String REGISTER_SUCCESS="register_success";
    public static String REGISTER_INVALID="register_invalid";
    public static String DELETE_SUCCESS="delete_success";
    public static String JOIN_SUCCESS="join_success";
    public static String JOIN_INVALID="join_invalid";
    public static String NO_SUCH_USER="no_such_user";
    public static String GRANT_SUCCESS="grant_success";
    public static String GRANT_INVALID="grant_invalid";
    public static String REVOKE_SUCCESS="revoke_success";
    public static String REVOKE_INVALID="revoke_invalid";
    public static String GRANTED="granted";
    public static String REVOKED="revoked";
    public static String CREATE_SUCCESS="create_success";
    public static String CREATE_INVALID="create_invalid";
    public static String REMOVE_SUCCESS="remove_success";
    public static String REMOVE_INVALID="remove_invalid";
    public static String KICKED="kicked";

    private static void load() throws FileNotFoundException {
        FileInputStream in = new FileInputStream(new File(MESSAGE_FILE));
        messages = (Map<String, String>)file.load(in);
    }

    @Suspendable
    public static void init() throws FileNotFoundException {
        file = new Yaml();
        load();
    }

    public static void reload() throws FileNotFoundException {
        load();
    }

    public static String message(String key) {
        return messages.get(key);
    }

    public static String format(String uname, String contents) {
        return uname + ": " + contents;
    }

    public static String formatPM(String uname, String contents) {
        return "(PM) " + format(uname, contents);
    }
}
