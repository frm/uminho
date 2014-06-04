
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
    private HashMap<Integer,Integer> distanceMS;
    
    //constructors public 
    public DistanceMilestones(){
        super();
        this.distanceMS = new HashMap<Integer,Integer>();
    }
    
    public DistanceMilestones(HashMap<Long,Integer> cms,HashMap<Integer,Integer> dms){
        super(cms);
        this.distanceMS = cloneDistanceMilestones(dms);
    }
    
    public DistanceMilestones(DistanceMilestones dms){
        super(dms);
        this.distanceMS = dms.getDistanceMilestones();
    }
    
    //setters & getters
    public HashMap<Integer,Integer> getDistanceMilestones()
    {return cloneDistanceMilestones(this.distanceMS);}
    
    public void setDistanceMS(HashMap<Integer, Integer> distanceMS)
    {this.distanceMS = cloneDistanceMilestones(distanceMS);}
    
    //methods
    public HashMap<Integer, Integer> cloneDistanceMilestones(HashMap<Integer,Integer> dm) {
        HashMap<Integer,Integer> aux = new HashMap<Integer,Integer>();
        for(Map.Entry<Integer,Integer> cms: dm.entrySet())
            aux.put(cms.getKey(), cms.getValue());
        return aux;
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
