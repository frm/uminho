
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
    
    public Kendo(Kendo k)
    {super(k);}
    
    public Kendo clone()
    {return new Kendo(this);}
    
    public int calculateCalories(long duration)
    {return (int) (duration*0.00008);}
}
