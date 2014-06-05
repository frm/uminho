
import java.util.TreeMap;
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
    private TreeMap<Integer,Long> altitude;
    private TreeMap<Long,Integer> reverseA;

    
    //constructors public
    public AltitudeMilestones(){
        super();
        this.altitude = new TreeMap<Integer,Long>();
        this.reverseA = new TreeMap<Long,Integer>();
        this.populateMilestones();
    }

    public AltitudeMilestones(TreeMap<Long,Integer> cms, TreeMap<Integer,Long> rC, TreeMap<Integer,Long> dms, TreeMap<Long,Integer> rD, TreeMap<Integer,Long> ams, TreeMap<Long,Integer> rA){
        super(cms, rC, dms, rD);
        this.altitude = cloneAltitudeMilestones(ams);
        this.reverseA = cloneReverseAltitudeMilestones(rA);
    }

    public AltitudeMilestones(AltitudeMilestones ams){
        super(ams);
        this.altitude = ams.getAltitudeMilestones();
        this.reverseA = ams.getReverseAltitudeMilestones();
    }

    //setters & getters
    public TreeMap<Integer,Long> getAltitudeMilestones()
    {return cloneAltitudeMilestones(this.altitude);}

    public TreeMap<Long,Integer> getReverseAltitudeMilestones()
    {return cloneReverseAltitudeMilestones(this.reverseA);}

    public void setAltitude(TreeMap<Integer, Long> altitude)
    {this.altitude = cloneAltitudeMilestones(altitude);}

    public void setReverseAltitude(TreeMap<Long,Integer> rA)
    {this.reverseA = cloneReverseAltitudeMilestones(rA);}

    //methods
    public TreeMap<Integer, Long> cloneAltitudeMilestones(TreeMap<Integer,Long> am) {
        TreeMap<Integer,Long> aux = new TreeMap<Integer,Long>();
        for(Map.Entry<Integer,Long> ams: am.entrySet())
            aux.put(ams.getKey(), ams.getValue());
        return aux;
    }

    public TreeMap<Long,Integer> cloneReverseAltitudeMilestones(TreeMap<Long,Integer> rm) {
        TreeMap<Long,Integer> aux = new TreeMap<Long,Integer>();
        for(Map.Entry<Long,Integer> rms: rm.entrySet())
            aux.put(rms.getKey(), rms.getValue());
        return aux;
    }
    
    public void populateMilestones(){
        super.populateMilestones();
        this.altitude.put(50,-1L);
        this.altitude.put(100,-1L);
        this.altitude.put(300,-1L);
        this.altitude.put(500,-1L);
        this.altitude.put(1000,-1L);
        this.reverseA.put(10L,-1);
        this.reverseA.put(30L,-1);
        this.reverseA.put(60L,-1);
        this.reverseA.put(120L,-1);
        this.reverseA.put(180L,-1);
    }

    public void addData(AltitudeActivity act){
        super.addData(act);
        long actDuration = act.getDuration();
        int actAltitude = act.getAltitude();

        for(Map.Entry<Integer,Long> pair : altitude.entrySet()){
            if(actAltitude >= pair.getKey()){
                long aux = ruleOfThree(actDuration/60000L, (long) actAltitude, pair.getKey());

                if(aux > pair.getValue())
                    altitude.put((int)pair.getKey(),aux);
            }
            else break;
        }
        
        for(Map.Entry<Long,Integer> pair : reverseA.entrySet()){
            if(actDuration >= pair.getValue()){
                long aux = ruleOfThree((long) actAltitude, actDuration/60000L, pair.getKey());

                if(aux > pair.getKey())
                    reverseA.put(pair.getKey(), (int)aux);
            }
            else break;
        }
    }

    //essentials
    public AltitudeMilestones clone() {
        return new AltitudeMilestones(this);
    }

    public String firsttoString(){
        StringBuilder result = new StringBuilder();

        for(Map.Entry<Integer,Long> pair: this.altitude.entrySet()){
            result.append( pair.getKey());
            result.append( " meters: ");
            
            if(pair.getValue() == -1)
                result.append(" No info\n");
            else
                result.append( StatEntry.formatMillis( pair.getValue() ));
        }
        
        return ( super.toString() + result.toString() );
    }
    
    public String secondtoString(){
        StringBuilder result = new StringBuilder();
        
        for(Map.Entry<Long,Integer> pair: this.reverseA.entrySet()){
            result.append( StatEntry.formatMillis( pair.getKey() ));
            result.append( " : ");
            
            if(pair.getValue() == -1)
                result.append(" No info\n");
            else
                result.append( pair.getValue());
        }
        
        return ( super.toString() + result.toString() );
    }

    public String toString(){
        return (firsttoString() + secondtoString());
    }
    public boolean equals(Object o){
        if (this == o) return true;

        if (o == null || this.getClass() != o.getClass()) return false;

        AltitudeMilestones ams = (AltitudeMilestones) o;
        AltitudeMilestones rms = (AltitudeMilestones) o;
        
        return (this.altitude.equals(ams.getAltitudeMilestones()) && this.reverseA.equals(rms.getReverseAltitudeMilestones()));
    }}

