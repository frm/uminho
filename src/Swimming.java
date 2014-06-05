
/**
 *
 * @author tiago
 */

import java.util.GregorianCalendar;

public class Swimming extends DistanceActivity {

    public Swimming() 
    {super();}
    
    public Swimming(GregorianCalendar date, long duration, int distance){
        super(date,duration,distance);
        this.setCalories(calculateCalories(duration, distance) );
    }
    
    public Swimming(DistanceActivity model){
        super(model);
        this.setCalories(calculateCalories(model.getDuration(), model.getDistance()) );
    }
    
    
    public Swimming(Swimming s)
    {super(s);}

    public Swimming clone()
    {return new Swimming(this);}
    
    public int calculateCalories(long duration, int distance)
    {return (int) ( (distance/duration)*900000.0) ;}
}

