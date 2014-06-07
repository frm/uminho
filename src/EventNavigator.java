
import java.util.ArrayList;


/**
 * Navigator for events
 * @author joaorodrigues
 */
public class EventNavigator extends Navigator<Event>{
    private FitnessUM app;

    /**
     * Empty constructor
     */
    public EventNavigator() {
        super();
        this.app = new FitnessUM();
    }

    /**
     * Parameterized constructor
     * @param list
     */
    public EventNavigator(ArrayList<Event> list) {
        super(list);
        this.app = new FitnessUM();
    }

    /**
     * Completely parameterized constructor
     * @param list
     * @param app
     */
    public EventNavigator(ArrayList<Event> list, FitnessUM app ){
        super(list);
        this.app = app;
    }


    @Override
    public void print(Event e) {
        System.out.println( e );
    }

    @Override
    public void select(Event e) {
        if (app.getUserController().userParticipatedIn( e.getId() ) )
            simulateDelete(e);        
        else
            if( Scan.yesNo("Are you sure you want to join the event?") )
                app.joinEvent(e);

    }


    @Override
    public String emptyMessage() {
        return "\nNo Events Available\n";
    }
    
    /**
     * Auxiliary method that either deletes the user from the event or simulates it
     * @param e 
     */
    private void simulateDelete(Event e) {
        int option = Scan.intInRange("Please select one of these options:\n1. Cancel participation\n2. Simulate", 1, 2);
        if (option == 1) {
            if ( Scan.yesNo("Are you sure you want to cancel your participation?") ){
                app.leaveEvent(e);
                remove(e);
            }
        }
        else
            app.simulateEvent(e);
    }


}