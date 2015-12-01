package util;

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
    public static String REGISTER_SUCCESS="register_success";
    public static String REGISTER_INVALID="register_invalid";
    public static String DELETE_SUCCESS="delete_success";

    private static void load() throws FileNotFoundException {
        FileInputStream in = new FileInputStream(new File(MESSAGE_FILE));
        messages = (Map<String, String>)file.load(in);
    }

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
}
