import java.io.Serializable;
import java.util.Comparator;


/**
 * Comparator for the events
 * @author joaorodrigues
 */
public class EventComparator implements Comparator<Event>, Serializable {
    
    @Override
    public int compare(Event e1, Event e2){
        int compare = (e1.getDate()).compareTo(e2.getDate());
        if( compare < 0) return 1;
        if( compare > 0) return -1;
        else return ( e1.getName().compareTo( e2.getName() ) );
        
   }
}
