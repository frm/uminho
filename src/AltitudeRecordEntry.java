
/**
 *
 * @author joaorodrigues
 */
public class AltitudeRecordEntry extends DistanceRecordEntry{
    private AltitudeMilestones altitudeMilestones;
    
    public AltitudeRecordEntry() {
        super();
        this.altitudeMilestones = new AltitudeMilestones();
    }

    public AltitudeRecordEntry(AltitudeMilestones altitudeMilestones, String n, Milestones ms, DistanceMilestones dms) {
        super(dms, n, ms);
        this.altitudeMilestones = altitudeMilestones.clone();
    }
    
    public AltitudeRecordEntry(AltitudeRecordEntry are){
        super(are);
        this.altitudeMilestones = are.getAltitudeMilestones();
    }
    
    public AltitudeRecordEntry(Activity act){
        super(act);
        this.altitudeMilestones.addData(act);
    }

    public AltitudeMilestones getAltitudeMilestones() {
        return altitudeMilestones.clone();
    }

    public void setAltitudeMilestones(AltitudeMilestones altitudeMilestones) {
        this.altitudeMilestones = altitudeMilestones.clone();
    }

    public void updateRecords(Activity act) {
        super.updateRecords(act); 
        this.altitudeMilestones.addData(act);
    }
    
    public AltitudeRecordEntry clone(AltitudeRecordEntry are){
        return new AltitudeRecordEntry(are);
    }

    public String toString() {
        return ( super.toString() + "\nAltitude Milestones: \n" + altitudeMilestones); 
    }
    
    
    
     
   
}
