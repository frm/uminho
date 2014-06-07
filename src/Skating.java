/**
 *
 * @author tiago
 */

import java.util.GregorianCalendar;

/**
 *
 * @author joaorodrigues
 */
public class Skating extends Activity {
    
    /**
     *
     */
    public Skating() 
    {super();}
    
    /**
     *
     * @param date
     * @param duration
     */
    public Skating(GregorianCalendar date, long duration){
        super(date, duration);
        this.setCalories( calculateCalories(duration) );
    }
    
    /**
     *
     * @param model
     */
    public Skating(Activity model){
        super(model);
        this.setCalories( calculateCalories(model.getDuration()) );
    }
    
    /**
     *
     * @param s
     */
    public Skating (Skating s)
    {super(s);}
    
    public Skating clone()
    {return new Skating(this);}
    
    /**
     *
     * @param duration
     * @return
     */
    public int calculateCalories(long duration){
        int randomVariance = (int) (Math.random()*20);
        return ((int) (duration*0.00006)) + randomVariance;
    }
}