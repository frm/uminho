
/**
 *
 * @author tiago
 */

import java.util.GregorianCalendar;

/**
 *
 * @author joaorodrigues
 */
public class Swimming extends DistanceActivity {

    /**
     *
     */
    public Swimming() 
    {super();}
    
    /**
     *
     * @param date
     * @param duration
     * @param distance
     */
    public Swimming(GregorianCalendar date, long duration, int distance){
        super(date,duration,distance);
        this.setCalories(calculateCalories(duration, distance) );
    }
    
    /**
     *
     * @param model
     */
    public Swimming(DistanceActivity model){
        super(model);
        this.setCalories(calculateCalories(model.getDuration(), model.getDistance()) );
    }
    
    /**
     *
     * @param s
     */
    public Swimming(Swimming s)
    {super(s);}

    public Swimming clone()
    {return new Swimming(this);}
    
    /**
     *
     * @param duration
     * @param distance
     * @return
     */
    public int calculateCalories(long duration, int distance){
        int randomVariance = (int) (Math.random()*20);
        return ((int) ( ((double)distance/(double)duration)*900000)) + randomVariance;
    }
}

