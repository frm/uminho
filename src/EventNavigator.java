
import java.util.ArrayList;


/**
 *
 * @author joaorodrigues
 */
public class EventNavigator extends Navigator<Event>{
    private FitnessUM app;

    /**
     *
     */
    public EventNavigator() {
        super();
        this.app = new FitnessUM();
    }

    /**
     *
     * @param list
     */
    public EventNavigator(ArrayList<Event> list) {
        super(list);
        this.app = new FitnessUM();
    }

    /**
     *
     * @param list
     * @param app
     */
    public EventNavigator(ArrayList<Event> list, FitnessUM app ){
        super(list);
        this.app = app;
    }

    /**
     *
     * @param e
     */
    public void print(Event e) {
        System.out.println( e );
    }

    /**
     *
     * @param e
     */
    public void select(Event e) {
        if (app.getUserController().userParticipatedIn( e.getId() ) )
            simulateDelete(e);        
        else
            if( Scan.yesNo("Are you sure you want to join the event?") )
                app.joinEvent(e);

    }

    /**
     *
     * @return
     */
    public String emptyMessage() {
        return "\nNo Events Available\n";
    }
    
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