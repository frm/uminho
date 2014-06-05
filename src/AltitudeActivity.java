import java.util.GregorianCalendar;

/**
 *
 * @author joaorodrigues
 */
public abstract class AltitudeActivity extends DistanceActivity{
    int altitude;
    
    //constructors

    /**
     *
     */
        public AltitudeActivity(){
       super();
       this.altitude = 0;
   }
   
    /**
     *
     * @param date
     * @param duration
     * @param distance
     * @param altitude
     */
    public AltitudeActivity(GregorianCalendar date, long duration, int distance, int altitude){
        super(date, duration, distance);
        this.altitude = altitude;
    }
   
    /**
     *
     * @param act
     */
    public AltitudeActivity(AltitudeActivity act) {
       super(act);
       this.altitude = act.getAltitude();
   }
   
    //setters
    void setAltitude(int a) {
        this.altitude = a;
    }
    
    //getters
    int getAltitude() {
        return this.altitude;
    }
    
    public String toString() {
        StringBuilder understring = new StringBuilder();
        understring.append("\nAltitude difference: ");
        understring.append(this.altitude);
        
        return super.toString() + understring.toString();
    }
    
    public boolean equals(Object o) {
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        AltitudeActivity act = (AltitudeActivity) o;
       
       return ( super.equals(o) && this.altitude == act.getAltitude() );
    }
}
