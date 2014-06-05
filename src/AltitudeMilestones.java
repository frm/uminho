
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
    private HashMap<Integer,Long> altitude;

    //constructors public
    public AltitudeMilestones(){
        super();
        this.altitude = new HashMap<Integer,Long>();
        this.populateMilestones();
    }

    public AltitudeMilestones(HashMap<Long,Integer> cms,HashMap<Integer,Long> dms,HashMap<Integer,Long> ams){
        super(cms, dms);
        this.altitude = cloneAltitudeMilestones(ams);
    }

    public AltitudeMilestones(AltitudeMilestones ams){
        super(ams);
        this.altitude = ams.getAltitudeMilestones();
    }

    //setters & getters
    public HashMap<Integer,Long> getAltitudeMilestones()
    {return cloneAltitudeMilestones(this.altitude);}

    public void setAltitude(HashMap<Integer, Long> altitude)
    {this.altitude = cloneAltitudeMilestones(altitude);}

    //methods
    public HashMap<Integer, Long> cloneAltitudeMilestones(HashMap<Integer,Long> am) {
        HashMap<Integer,Long> aux = new HashMap<Integer,Long>();
        for(Map.Entry<Integer,Long> ams: am.entrySet())
            aux.put(ams.getKey(), ams.getValue());
        return aux;
    }

    public void populateMilestones(){
        super.populateMilestones();
        this.altitude.put(50,-1L);
        this.altitude.put(100,-1L);
        this.altitude.put(300,-1L);
        this.altitude.put(500,-1L);
        this.altitude.put(1000,-1L);
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
    }

    //essentials
    public AltitudeMilestones clone() {
        return new AltitudeMilestones(this);
    }

    public String toString(){
        StringBuilder result = new StringBuilder();

        for(Map.Entry<Integer,Long> pair: this.altitude.entrySet()){
            result.append( pair.getKey());
            result.append( " meters: ");
            result.append( StatEntry.formatMillis( pair.getValue() ));
        }
        return ( super.toString() + result.toString() );
    }

    public boolean equals(Object o){
        if (this == o) return true;

        if (o == null || this.getClass() != o.getClass()) return false;

        AltitudeMilestones dms = (AltitudeMilestones) o;

        return this.altitude.equals(dms.getDistanceMilestones());
    }}
