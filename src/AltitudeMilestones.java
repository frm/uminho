
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
public class AltitudeMilestones extends DistanceMilestones{
    private HashMap<Integer,Integer> altitudeMS;
    
    //constructors public 
    public AltitudeMilestones(){
        super();
        this.altitudeMS = new HashMap<Integer,Integer>();
    }
    
    public AltitudeMilestones(HashMap<Long,Integer> cms,HashMap<Integer,Integer> dms,HashMap<Integer,Integer> ams){
        super(cms, dms);
        this.altitudeMS = cloneAltitudeMilestones(ams);
    }
    
    public AltitudeMilestones(AltitudeMilestones ams){
        super(ams);
        this.altitudeMS = ams.getAltitudeMilestones();
    }
    
    //setters & getters
    public HashMap<Integer,Integer> getAltitudeMilestones()
    {return cloneAltitudeMilestones(this.altitudeMS);}
    
    public void setAltitudeMS(HashMap<Integer, Integer> altitudeMS)
    {this.altitudeMS = cloneAltitudeMilestones(altitudeMS);}
    
    //methods
    public HashMap<Integer, Integer> cloneAltitudeMilestones(HashMap<Integer,Integer> dm) {
        HashMap<Integer,Integer> aux = new HashMap<Integer,Integer>();
        for(Map.Entry<Integer,Integer> cms: dm.entrySet())
            aux.put(cms.getKey(), cms.getValue());
        return aux;
    }
    
    //essentials
    public AltitudeMilestones clone()
    {return new AltitudeMilestones(this);}
    
    public String toString(){
        StringBuilder understring = new StringBuilder();
        understring.append("\nAltitude Milestones: ");
        understring.append(this.altitudeMS.toString());
        
        return super.toString() + understring.toString();
    }
    
    public boolean equals(Object o){
        if (this == o) return true;
        
        if (o == null || this.getClass() != o.getClass()) return false;
        
        AltitudeMilestones dms = (AltitudeMilestones) o;
        
        return this.altitudeMS.equals(dms.getDistanceMilestones());
    }}
