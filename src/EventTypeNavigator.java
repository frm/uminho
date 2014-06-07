
import java.util.ArrayList;


/**
 * Navigator for events of a given type
 * @author joaorodrigues
 */
public class EventTypeNavigator extends Navigator<String>{
    private FitnessUM app;
    
    /**
     * Empty constructor
     */
    public EventTypeNavigator() {
        super();
    }
    
    /**
     * Parameterized constructor
     * @param list
     * @param app
     */
    public EventTypeNavigator(ArrayList<String> list, FitnessUM app ){
        super(list);
        this.app = app;
    }

    /**
     * Returns the app
     * @return
     */
    public FitnessUM getApp() {
        return app;
    }

    /**
     * Sets the app
     * @param app
     */
    public void setApp(FitnessUM app) {
        this.app = app;
    }

    @Override
    public void print(String s) {
        System.out.println( s );
    }
    

    @Override
    public void select(String s){ 
        app.getEventInfo(s);
    }
    
    @Override
    public String emptyMessage() {
        return "\nThe app has no Activities available\n";
    }
}
