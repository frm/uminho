/*
 * Exception to be thrown when event that is being accessed does not exist
 */

/**
 *
 * @author frmendes
 */
public class InexistingEventException extends Exception {
    
    /**
     *
     */
    public InexistingEventException() {
        super();
    }
    
    /**
     *
     * @param msg
     */
    public InexistingEventException(String msg) {
        super(msg);
    }
}
