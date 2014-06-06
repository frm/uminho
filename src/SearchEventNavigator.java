
import java.util.ArrayList;

/**
 *
 * @author joaorodrigues
 */
public class SearchEventNavigator extends Navigator<String>{
    private EventController ec;
    
    public SearchEventNavigator() {
        super();
        this.ec = new EventController();
    }
    
    public SearchEventNavigator(ArrayList<String> list, EventController eventc ){
        super(list);
        this.ec = eventc.clone();
    }

    public void print(String s) {
        System.out.println( s );
    }
    
    public void select(String s){
        System.out.println( ec.getEventByName(s));
    }
    
    public String emptyMessage() {
        return "\nUser has no activities\n";
    }
}
