
import java.util.HashMap;
import java.util.Map;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author tiago
 */
public class DistanceMilestones extends Milestones{
    private HashMap<Integer,Long> distanceMS;
    
    //constructors public 
    public DistanceMilestones(){
        super();
        this.distanceMS = new HashMap<Integer,Long>();
    }
    
    public DistanceMilestones(HashMap<Long,Integer> cms,HashMap<Integer,Long> dms){
        super(cms);
        this.distanceMS = cloneDistanceMilestones(dms);
    }
    
    public DistanceMilestones(DistanceMilestones dms){
        super(dms);
        this.distanceMS = dms.getDistanceMilestones();
    }
    
    //setters & getters
    public HashMap<Integer,Long> getDistanceMilestones()
    {return cloneDistanceMilestones(this.distanceMS);}
    
    public void setDistanceMS(HashMap<Integer,Long> distanceMS)
    {this.distanceMS = cloneDistanceMilestones(distanceMS);}
    
    //methods
    public HashMap<Integer,Long> cloneDistanceMilestones(HashMap<Integer,Long> dm) {
        HashMap<Integer,Long> aux = new HashMap<Integer,Long>();
        for(Map.Entry<Integer,Long> dms: dm.entrySet())
            aux.put(dms.getKey(), dms.getValue());
        return aux;
    }
    
    public void populateMilestones(){
        super.populateMilestones();
        this.distanceMS.put(1000,(long)-1);
        this.distanceMS.put(5000,(long)-1);
        this.distanceMS.put(10000,(long)-1);
        this.distanceMS.put(20000,(long)-1);
    }
    
    public void addData(DistanceActivity act){
        super.addData(act);
        long actDuration = act.getDuration();
        int actDistance = act.getDistance();
        
        for(Map.Entry<Integer,Long> pair : distanceMS.entrySet()){
            if(actDistance >= pair.getKey()){
                actDuration = (actDuration*(pair.getKey()))/actDistance;
                
                if(actDuration > pair.getValue())
                    distanceMS.put((int)pair.getKey(),(long)actDuration);
            }
            else break;
        }
    }
    //essentials
    public DistanceMilestones clone()
    {return new DistanceMilestones(this);}
    
    public String toString(){
        StringBuilder understring = new StringBuilder();
        understring.append("\nDistance Milestones: ");
        understring.append(this.distanceMS.toString());
        
        return super.toString() + understring.toString();
    }
    
    public boolean equals(Object o){
        if (this == o) return true;
        
        if (o == null || this.getClass() != o.getClass()) return false;
        
        DistanceMilestones dms = (DistanceMilestones) o;
        
        return this.distanceMS.equals(dms.getDistanceMilestones());
    }
}
