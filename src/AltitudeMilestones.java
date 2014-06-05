
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
    private HashMap<Integer,Long> altitudeMS;
    
    //constructors public 
    public AltitudeMilestones(){
        super();
        this.altitudeMS = new HashMap<Integer,Long>();
        this.populateMilestones();
    }
    
    public AltitudeMilestones(HashMap<Long,Integer> cms,HashMap<Integer,Long> dms,HashMap<Integer,Long> ams){
        super(cms, dms);
        this.altitudeMS = cloneAltitudeMilestones(ams);
    }
    
    public AltitudeMilestones(AltitudeMilestones ams){
        super(ams);
        this.altitudeMS = ams.getAltitudeMilestones();
    }
    
    //setters & getters
    public HashMap<Integer,Long> getAltitudeMilestones()
    {return cloneAltitudeMilestones(this.altitudeMS);}
    
    public void setAltitudeMS(HashMap<Integer, Long> altitudeMS)
    {this.altitudeMS = cloneAltitudeMilestones(altitudeMS);}
    
    //methods
    public HashMap<Integer, Long> cloneAltitudeMilestones(HashMap<Integer,Long> am) {
        HashMap<Integer,Long> aux = new HashMap<Integer,Long>();
        for(Map.Entry<Integer,Long> ams: am.entrySet())
            aux.put(ams.getKey(), ams.getValue());
        return aux;
    }
    
    public void populateMilestones(){
        super.populateMilestones();
        this.altitudeMS.put(50,-1L);
        this.altitudeMS.put(100,-1L);
        this.altitudeMS.put(300,-1L);
        this.altitudeMS.put(500,-1L);
        this.altitudeMS.put(1000,-1L);
    }
    
    public void addData(AltitudeActivity act){
        super.addData(act);
        long actDuration = act.getDuration();
        int actAltitude = act.getAltitude();
        
        for(Map.Entry<Integer,Long> pair : altitudeMS.entrySet()){
            if(actAltitude >= pair.getKey()){
                actDuration = (actDuration*(pair.getKey()))/actAltitude;
                
                if(actDuration > pair.getValue())
                    altitudeMS.put((int)pair.getKey(),(long)actDuration);
            }
            else break;
        }
    }
    
    //essentials
    public AltitudeMilestones clone()
    {return new AltitudeMilestones(this);}
    
    public String toString(){
        return super.toString() + this.altitudeMS.toString();
    }
    
    public boolean equals(Object o){
        if (this == o) return true;
        
        if (o == null || this.getClass() != o.getClass()) return false;
        
        AltitudeMilestones dms = (AltitudeMilestones) o;
        
        return this.altitudeMS.equals(dms.getDistanceMilestones());
    }}
