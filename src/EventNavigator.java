
import java.util.ArrayList;


/**
 *
 * @author joaorodrigues
 */
public class EventNavigator extends Navigator<Event>{
    private FitnessUM app;

    public EventNavigator() {
        super();
        this.app = new FitnessUM();
    }

    public EventNavigator(ArrayList<Event> list) {
        super(list);
        this.app = new FitnessUM();
    }

    public EventNavigator(ArrayList<Event> list, FitnessUM app ){
        super(list);
        this.app = app;
    }

    public void print(Event e) {
        System.out.println( e );
    }
    public void select(Event e) {
        if (app.getUserController().userParticipatedIn( e.getId() ) ) {
            if ( Scan.yesNo("Are you sure you want to cancel your participation?") )
                app.leaveEvent(e);
        }
        
        else
            if( Scan.yesNo("Are you sure you want to join the event?") )
                app.joinEvent(e);

    }

    public String emptyMessage() {
        return "\nNo Events Available\n";
    }


}