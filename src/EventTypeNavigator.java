
import java.util.ArrayList;


/**
 *
 * @author joaorodrigues
 */
public class EventTypeNavigator extends Navigator<String>{
    private FitnessUM app;
    
    /**
     *
     */
    public EventTypeNavigator() {
        super();
    }
    
    /**
     *
     * @param list
     * @param app
     */
    public EventTypeNavigator(ArrayList<String> list, FitnessUM app ){
        super(list);
        this.app = app;
    }

    /**
     *
     * @return
     */
    public FitnessUM getApp() {
        return app;
    }

    /**
     *
     * @param app
     */
    public void setApp(FitnessUM app) {
        this.app = app;
    }
    
    /**
     *
     * @param s
     */
    @Override
    public void print(String s) {
        System.out.println( s );
    }
    
    /**
     *
     * @param s
     */
    @Override
    public void select(String s){ 
        app.getEventInfo(s);
    }
    
    /**
     *
     * @return
     */
    @Override
    public String emptyMessage() {
        return "\nThe app has no Activities available\n";
    }
}
