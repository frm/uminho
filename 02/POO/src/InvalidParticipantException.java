/*
 * Exception to be thrown when a User who tries to participate in an event has invalid content
 */

/**
 *
 * @author frmendes
 */
public class InvalidParticipantException extends Exception {
    
    /**
     *
     */
    public InvalidParticipantException() {
        super();
    }
    
    /**
     *
     * @param msg
     */
    public InvalidParticipantException(String msg) {
        super(msg);
    }
    
}
