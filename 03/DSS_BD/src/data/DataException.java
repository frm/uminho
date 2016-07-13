/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package data;

import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author frmendes
 */
public class DataException extends Exception{
    private static Map<String, String> messageBag = new HashMap<String, String>() {{
      put("UserNotFound", "We don't seem to have any record of that given user");
    }};
    
    public DataException() {
        super();
    }
    
    public DataException(String message, Throwable cause) {
        super(message, cause);
    }
    
    public DataException(Throwable cause) {
        super(cause);
    }
    
    public DataException(String className) {
        super(DataException.messageBag.get(className));
    }
    
    public DataException(String className, String message) {
        super(DataException.messageBag.get(className));
        System.err.println(message);
    }
    
    public DataException(String className, String message, Throwable cause) {
        super(DataException.messageBag.get(className), cause);
        System.err.println(message);
    }
}
