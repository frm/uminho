
/**
 *
 * @author tiago
 */

import java.util.GregorianCalendar;

public class Kendo extends Activity {

    public Kendo() 
    {super();}
    
    public Kendo(GregorianCalendar date, long duration){
        super(date, duration);
        this.setCalories( calculateCalories(duration) );
    }
    
    public Kendo(Activity model){
        super(model);
        this.setCalories( calculateCalories(model.getDuration()) );
    }
    
    public Kendo(Kendo k)
    {super(k);}
    
    public Kendo clone()
    {return new Kendo(this);}
    
    public int calculateCalories(long duration){        
        int randomVariance = (int) (Math.random()*20);
        return ((int) (duration*0.00008)) + randomVariance;
    }
}
