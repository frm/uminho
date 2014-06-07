


/**
 *
 * @author joaorodrigues
 */
public class AltitudeStatEntry extends DistanceStatEntry{
    private int totalAltitude;
    private double avgAltitude;
    
    /**
     *
     */
    public AltitudeStatEntry(){
        super();
        this.totalAltitude = 0;
        this.avgAltitude = 0.0;
    }

    /**
     *
     * @param totalAltitude
     * @param avgAltitude
     */
    public AltitudeStatEntry(int totalAltitude, double avgAltitude) {
        this.totalAltitude = totalAltitude;
        this.avgAltitude = avgAltitude;
    }

    /**
     *
     * @param name
     * @param calories
     * @param duration
     * @param totalDistance
     * @param avgDistance
     * @param totalAltitude
     * @param avgAltitude
     */
    public AltitudeStatEntry(String name, int calories, long duration, int totalDistance, double avgDistance, int totalAltitude, double avgAltitude) {
        super(name, calories, duration, totalDistance, avgDistance);
        this.totalAltitude = totalAltitude;
        this.avgAltitude = avgAltitude;
    }

    /**
     *
     * @param name
     * @param totalCalories
     * @param avgCalories
     * @param totalDuration
     * @param avgDuration
     * @param nrEntries
     * @param totalDistance
     * @param avgDistance
     * @param totalAltitude
     * @param avgAltitude
     */
    public AltitudeStatEntry(String name, int totalCalories, int avgCalories, long totalDuration, long avgDuration, int nrEntries, int totalDistance, double avgDistance,  int totalAltitude, double avgAltitude) {
        super(name, totalCalories, avgCalories, totalDuration, avgDuration, nrEntries, totalDistance, avgDistance);
        this.totalAltitude = totalAltitude;
        this.avgAltitude = avgAltitude;
    }

    /**
     *
     * @param act
     */
    public AltitudeStatEntry(AltitudeActivity act) {
        super(act);
        this.totalAltitude = 0;
        this.avgAltitude = 0.0;
        
        updateStat(act);
    }
    
    /**
     *
     * @param ase
     */
    public AltitudeStatEntry(AltitudeStatEntry ase){
        super(ase);
        
        this.totalAltitude = ase.getTotalAltitude();
        this.avgAltitude = ase.getAvgAltitude();
    }
    
    /**
     *
     * @param act
     */
    public void updateStat(AltitudeActivity act){
        
        this.totalAltitude += act.getAltitude();
        this.avgAltitude = this.totalAltitude/this.getNrEntries();
    }
    
    /**
     *
     * @param calories
     * @param duration
     * @param distance
     * @param altitude
     */
    public void updateStat(int calories, long duration,int distance, int altitude){
        
        this.totalAltitude += altitude;
        this.avgAltitude = this.totalAltitude/this.getNrEntries();
    }
    
    /**
     *
     * @return
     */
    public int getTotalAltitude() {
        return totalAltitude;
    }

    /**
     *
     * @return
     */
    public double getAvgAltitude() {
        return avgAltitude;
    }

    /**
     *
     * @param totalAltitude
     */
    public void setTotalAltitude(int totalAltitude) {
        this.totalAltitude = totalAltitude;
    }

    /**
     *
     * @param avgAltitude
     */
    public void setAvgAltitude(double avgAltitude) {
        this.avgAltitude = avgAltitude;
    }
    
    
    public AltitudeStatEntry clone() {
       return (new AltitudeStatEntry(this));
    }
    
    public String toString(){
        StringBuilder result = new StringBuilder();
        result.append("\nTotal altitude: ");
        result.append(this.totalAltitude);
        result.append("\nAverage altitude per session: ");
        result.append(this.avgAltitude);
        
        return super.toString() + result.toString();
    }
    
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        AltitudeStatEntry ase = (AltitudeStatEntry) o;
       
       return ( super.equals(o) && this.totalAltitude == ase.getTotalAltitude() && this.avgAltitude == ase.getAvgAltitude() );
   }
    
}
