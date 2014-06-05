
import java.util.ArrayList;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author joaorodrigues
 */
public class ActivityNavigator extends Navigator<Activity>{
    private FitnessUM app;
    
    public ActivityNavigator() {
        super();
        this.app = new FitnessUM();
    }
    
    public ActivityNavigator(ArrayList<Activity> list) {
        super(list);
        this.app = new FitnessUM();
    }
    
    public ActivityNavigator( FitnessUM app, ArrayList<Activity> list ){
        super(list);
        this.app = app;
    }
    
    public void print(Activity act) {
        System.out.println( act );
    }
    
    public void select(Activity act){ 
        if( Scan.yesNo("Are you sure you want to delete the activity?")){
            app.removeActivity(act);
            super.remove(act);
        }
    }


    
    
    
    public String emptyMessage() {
        return "\nUser has no activities\n";
    }
}
