/**
 *
 * @author tiago
 */

import java.util.GregorianCalendar;

public class Skating extends Activity {
    
    public Skating() 
    {super();}
    
    public Skating(GregorianCalendar date, GregorianCalendar duration){
        super(date, duration);
        this.setCalories( calculateCalories(duration) );
    }
    
    public Skating (Skating s)
    {super(s);}
    
    public Skating clone()
    {return this.clone();}
    
    public double calculateCalories(GregorianCalendar duration)
    {return (double) duration.getTimeInMillis()*0.00006;}
}