
import java.util.ArrayList;

/**
 *
 * @author joaorodrigues
 */
public class SearchEventNavigator extends Navigator<String>{
    private EventController ec;
    
    /**
     *
     */
    public SearchEventNavigator() {
        super();
        this.ec = new EventController();
    }
    
    /**
     *
     * @param list
     * @param eventc
     */
    public SearchEventNavigator(ArrayList<String> list, EventController eventc ){
        super(list);
        this.ec = eventc.clone();
    }

    /**
     *
     * @param s
     */
    public void print(String s) {
        System.out.println(s);
    }
    
    /**
     *
     * @param s
     */
    public void select(String s){
        System.out.println( ec.getEventByName(s) );
    }
    
    /**
     *
     * @return
     */
    public String emptyMessage() {
        return "\nNo events available\n";
    }
}
