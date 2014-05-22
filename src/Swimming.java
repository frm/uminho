
/**
 *
 * @author tiago
 */

import java.util.GregorianCalendar;

public class Swimming extends DistanceActivity {

    public Swimming() 
    {super();}
    
    public Swimming(GregorianCalendar date, GregorianCalendar duration, int distance){
        super(date,duration,distance);
        this.setCalories(calculateCalories(duration, distance) );
    }
    
    public Swimming(Swimming s)
    {super(s);}

    public Swimming clone()
    {return new Swimming(this);}
    
    public double calculateCalories(GregorianCalendar duration, int distance)
    {return (double)(distance/duration.getTimeInMillis())*900000;}
}

