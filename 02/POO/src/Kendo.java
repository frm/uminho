
/**
 *
 * @author tiago
 */

import java.util.GregorianCalendar;

/**
 *
 * @author joaorodrigues
 */
public class Kendo extends Activity {

    /**
     *
     */
    public Kendo() 
    {super();}
    
    /**
     *
     * @param date
     * @param duration
     */
    public Kendo(GregorianCalendar date, long duration){
        super(date, duration);
        this.setCalories( calculateCalories(duration) );
    }
    
    /**
     *
     * @param model
     */
    public Kendo(Activity model){
        super(model);
        this.setCalories( calculateCalories(model.getDuration()) );
    }
    
    /**
     *
     * @param k
     */
    public Kendo(Kendo k)
    {super(k);}
    
    public Kendo clone()
    {return new Kendo(this);}
    
    /**
     *
     * @param duration
     * @return
     */
    public int calculateCalories(long duration){        
        int randomVariance = (int) (Math.random()*20);
        return ((int) (duration*0.00008)) + randomVariance;
    }
}
