import java.util.GregorianCalendar; 

/**Activity that has distance
 *
 * @author joaorodrigues
 */
public abstract class DistanceActivity extends Activity{
    int distance;
    
    //constructors

    /**
     *
     */
        public DistanceActivity(){
        super();
        this.distance = 0;
    }
    
    /**
     *
     * @param date
     * @param duration
     * @param distance
     */
    public DistanceActivity(GregorianCalendar date, long duration, int distance) {
        super(date, duration);
        this.distance = distance;
    }
    
    /**
     *
     * @param ad
     */
    public DistanceActivity(DistanceActivity ad) {
        super(ad);
        this.distance = ad.getDistance();
    }
    
    //setters

    /**
     *
     * @param d
     */
        public void setDistance(int d) {
        this.distance = d;
    }
    
    //getters

    /**
     *
     * @return
     */
        public int getDistance() {
        return this.distance;
    }
    
    
    //essentials
    
    @Override
    public abstract DistanceActivity clone();
    
    @Override
    public String toString(){
        StringBuilder understring = new StringBuilder();
        understring.append("\nDistance: ");
        understring.append(this.distance);
        
        return super.toString() + understring.toString();
    }
    
    @Override
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        DistanceActivity ad = (DistanceActivity) o;
       
       return ( super.equals(o) && this.distance == ad.getDistance() );
   }
}
