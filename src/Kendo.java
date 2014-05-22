
/**
 *
 * @author tiago
 */

import java.util.GregorianCalendar;

public class Kendo extends Activity {

    public Kendo() 
    {super();}
    
    public Kendo(GregorianCalendar date, GregorianCalendar duration){
        super(date, duration);
        this.setCalories( calculateCalories(duration) );
    }
    
    public Kendo(Kendo k)
    {super(k);}
    
    public Kendo clone()
    {return new Kendo(this);}
    
    public double calculateCalories(GregorianCalendar duration)
    {return (double) duration.getTimeInMillis()*0.00008;}
}
