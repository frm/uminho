/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package data;

import java.util.Map;

/**
 *
 * @author frmendes
 */
public class UserNotFoundException extends DataException {
    
    public UserNotFoundException() {
        super("UserNotFound");
    }
    
    public UserNotFoundException(String message) {
        super("UserNotFound", message);
    }
    
    public UserNotFoundException(Map<String, String> params) {
        super("UserNotFound", "Couldn't find referred user. Query parameters: " + params.toString());
    }
}
