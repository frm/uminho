
import java.util.ArrayList;

/**Navigates the activities
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
     * @param app used to call FitnessUM methods
     * @param list
     */
    public ActivityNavigator( FitnessUM app, ArrayList<Activity> list ){
        super(list);
        this.app = app;
    }
    
    
    @Override
    public void print(Activity act) {
        System.out.println( act );
    }
    
   
    @Override
    public void select(Activity act){ 
        if( Scan.yesNo("Are you sure you want to delete the activity?")){
            app.removeActivity(act);
            super.remove(act);
        }
    }

    
    @Override
    public String emptyMessage() {
        return "\nUser has no activities\n";
    }
}
