/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author frmendes
 */
public class InexistingUserException extends Exception {

    
    public InexistingUserException() {
        super();
    }
    
    public InexistingUserException(String msg) {
        super(msg);
    }
}
