
import java.util.ArrayList;

/**
 *
 * @author joaorodrigues
 */
public class ActivityNavigator extends Navigator<Activity>{
    private FitnessUM app;
    
    /**
     *
     */
    public ActivityNavigator() {
        super();
        this.app = new FitnessUM();
    }
    
    /**
     *
     * @param list
     */
    public ActivityNavigator(ArrayList<Activity> list) {
        super(list);
        this.app = new FitnessUM();
    }
    
    /**
     *
     * @param app
     * @param list
     */
    public ActivityNavigator( FitnessUM app, ArrayList<Activity> list ){
        super(list);
        this.app = app;
    }
    
    /**
     *
     * @param act
     */
    public void print(Activity act) {
        System.out.println( act );
    }
    
    /**
     *
     * @param act
     */
    public void select(Activity act){ 
        if( Scan.yesNo("Are you sure you want to delete the activity?")){
            app.removeActivity(act);
            super.remove(act);
        }
    }

    /**
     *
     * @return
     */
    public String emptyMessage() {
        return "\nUser has no activities\n";
    }
}
