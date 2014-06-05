/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author joaorodrigues
 */
public class DistanceRecordEntry extends RecordEntry{
    private DistanceMilestones distanceMilestones;
    
    public DistanceRecordEntry() {
        super();
        this.distanceMilestones = new DistanceMilestones();
    }

    public DistanceRecordEntry(DistanceMilestones distanceMilestones, String n, Milestones ms) {
        super(n, ms);
        this.distanceMilestones = distanceMilestones.clone();
    }
    
    public DistanceRecordEntry(DistanceRecordEntry dre){
        super(dre);
        this.distanceMilestones = dre.getDistanceMilestones();
    }
    
    public DistanceRecordEntry(Activity act){
        super(act);
        this.distanceMilestones = new DistanceMilestones();
        this.distanceMilestones.addData(act);
    }

    public DistanceMilestones getDistanceMilestones() {
        return distanceMilestones.clone();
    }

    public void setDistanceMilestones(DistanceMilestones distanceMilestones) {
        this.distanceMilestones = distanceMilestones.clone();
    }

    @Override
    public void updateRecords(Activity act) {
        super.updateRecords(act); 
        this.distanceMilestones.addData(act);
    }
    
    public DistanceRecordEntry clone(DistanceRecordEntry dre){
        return new DistanceRecordEntry(dre);
    }

    @Override
    public String toString() {
        return ( super.toString() + "\n##Distance Milestones## \n" + distanceMilestones.toString()); 
    }
    
    
    
     
   
}
