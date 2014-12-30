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
public class VolunteerNotFoundException extends DataException {
    
    public VolunteerNotFoundException() {
        super("VolunteerNotFound");
    }
    
    public VolunteerNotFoundException(String message) {
        super("VolunteerNotFound", message);
    }
    
    public VolunteerNotFoundException(Map<String, String> params) {
        super("VolunteerNotFound", "Couldn't find referred volunteer. Query parameters: " + params.toString());
    }
}
