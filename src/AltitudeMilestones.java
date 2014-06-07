
import java.util.Iterator;
import java.util.TreeMap;
import java.util.Map;

/**
 *
 * @author tiago
 */
public class AltitudeMilestones extends DistanceMilestones{
    private TreeMap<Long,Integer> altitude;
    private TreeMap<Integer,Long> reverseA;

    
    //constructors public

    /**
     *
     */
        public AltitudeMilestones(){
        super();
        populateMilestones();
    }
    
    /**
     *
     * @param act
     */
    public AltitudeMilestones(AltitudeActivity act){
        super(act);
        populateMilestones();
        this.addData(act);
    }

    /**
     *
     * @param cms
     * @param rC
     * @param dms
     * @param rD
     * @param ams
     * @param rA
     */
    public AltitudeMilestones(TreeMap<Long,Integer> cms, TreeMap<Integer,Long> rC, TreeMap<Long,Integer> dms, TreeMap<Integer,Long> rD, TreeMap<Long,Integer> ams, TreeMap<Integer,Long> rA){
        super(cms, rC, dms, rD);
        this.altitude = cloneAltitudeMilestones(ams);
        this.reverseA = cloneReverseAltitudeMilestones(rA);
    }

    /**
     *
     * @param ams
     */
    public AltitudeMilestones(AltitudeMilestones ams){
        super(ams);
        this.altitude = ams.getAltitudeMilestones();
        this.reverseA = ams.getReverseAltitudeMilestones();
    }

    //setters & getters

    /**
     *
     * @return
     */
        public TreeMap<Long,Integer> getAltitudeMilestones()
    {return cloneAltitudeMilestones(this.altitude);}

    /**
     *
     * @return
     */
    public TreeMap<Integer,Long> getReverseAltitudeMilestones()
    {return cloneReverseAltitudeMilestones(this.reverseA);}

    /**
     *
     * @param altitude
     */
    public void setAltitude(TreeMap<Long,Integer> altitude)
    {this.altitude = cloneAltitudeMilestones(altitude);}

    /**
     *
     * @param rA
     */
    public void setReverseAltitude(TreeMap<Integer,Long> rA)
    {this.reverseA = cloneReverseAltitudeMilestones(rA);}

    //methods

    /**
     *
     * @param am
     * @return
     */
        public TreeMap<Long,Integer> cloneAltitudeMilestones(TreeMap<Long,Integer> am) {
        TreeMap<Long,Integer> aux = new TreeMap<Long,Integer>();
        for(Map.Entry<Long,Integer> ams: am.entrySet())
            aux.put(ams.getKey(), ams.getValue());
        return aux;
    }

    /**
     *
     * @param rm
     * @return
     */
    public TreeMap<Integer,Long> cloneReverseAltitudeMilestones(TreeMap<Integer,Long> rm) {
        TreeMap<Integer,Long> aux = new TreeMap<Integer,Long>();
        for(Map.Entry<Integer,Long> rms: rm.entrySet())
            aux.put(rms.getKey(), rms.getValue());
        return aux;
    }
    
    /**
     *
     */
    public void populateMilestones(){
        super.populateMilestones();
        this.altitude = new TreeMap<Long,Integer>();
        this.reverseA = new TreeMap<Integer,Long>();
        this.altitude.put(10L,-1);
        this.altitude.put(30L,-1);
        this.altitude.put(60L,-1);
        this.altitude.put(120L,-1);
        this.altitude.put(180L,-1);
        this.reverseA.put(10,Long.MAX_VALUE);
        this.reverseA.put(50,Long.MAX_VALUE);
        this.reverseA.put(100,Long.MAX_VALUE);
        this.reverseA.put(300,Long.MAX_VALUE);
        this.reverseA.put(500,Long.MAX_VALUE);
    }

    /**
     *
     * @param act
     */
    public void addAltitudeData(AltitudeActivity act){
        int actMinDuration = (int) (act.getDuration()/60000L);
        int actAltitude = act.getAltitude();
        
        Iterator it = altitude.entrySet().iterator();
        boolean hasFinished = false;
        
        while( it.hasNext() && !hasFinished ) {
            Map.Entry<Long, Integer> pair = (Map.Entry<Long, Integer>)it.next();
            
            if ( actMinDuration >= pair.getKey() ) {
                int aux = (int) ruleOfThree(actMinDuration, (long)actAltitude, pair.getKey() );
                if( aux > pair.getValue() )
                    altitude.put(pair.getKey(), aux);
            }
            else hasFinished = true;
        }
    }

    /**
     *
     * @param act
     */
    public void addTimeAltitudeData(AltitudeActivity act){
        int actMinDuration = (int) (act.getDuration()/60000L);
        int actAltitude = act.getAltitude();
        
        Iterator it = reverseA.entrySet().iterator();
        boolean hasFinished = false;
        
        while( it.hasNext() && !hasFinished) {
            Map.Entry<Integer, Long> pair = (Map.Entry<Integer, Long>)it.next();
            
            if( actAltitude >= pair.getKey() ) {
                int aux = (int) ruleOfThree( (long) actAltitude, actMinDuration, pair.getKey() );
                if(aux < pair.getValue())
                    reverseA.put(pair.getKey(), (long)aux);
            }
            
            else hasFinished = false;
        }
    }
    
    /**
     *
     * @param act
     */
    public void addData(AltitudeActivity act){
        super.addData(act);
        addAltitudeData(act);
        addTimeAltitudeData(act);
    }
    //essentials
    public AltitudeMilestones clone() {
        return new AltitudeMilestones(this);
    }
    
    /**
     *
     * @return
     */
    public String firsttoStringA(){
        StringBuilder result = new StringBuilder();
        
        for(Map.Entry<Long,Integer> pair: this.altitude.entrySet()){

            if(pair.getKey() < 60){
                result.append(pair.getKey());
                result.append(" minutes: ");
            }
            else {
                result.append( (pair.getKey())/60 );
                result.append(" hour(s): ");
            }
            
            if(pair.getValue() == -1)
                result.append(" No info\n");
            else{
                result.append(pair.getValue());
                result.append(" m\n");
            }
        }
        
        return result.toString();
    }

    /**
     *
     * @return
     */
    public String secondtoStringA(){
        StringBuilder result = new StringBuilder();

        result.append("\n");
        for(Map.Entry<Integer,Long> pair: this.reverseA.entrySet()){
            result.append( pair.getKey());
            result.append( " m: ");

            if(pair.getValue() == Long.MAX_VALUE)
                result.append(" No info\n");
            else{
                result.append(pair.getValue());
                result.append(" minutes\n");
            }
        }
        
        return result.toString();
    }
    
    public String toString(){
        StringBuilder sb = new StringBuilder();
        
        sb.append(super.toString());
        sb.append("\n###Altitude Milestones###\n");
        sb.append(firsttoStringA());
        sb.append(secondtoStringA());

        return sb.toString();
    }
    
    public boolean equals(Object o){
        if (this == o) return true;

        if (o == null || this.getClass() != o.getClass()) return false;

        AltitudeMilestones ams = (AltitudeMilestones) o;
        AltitudeMilestones rms = (AltitudeMilestones) o;
        
        return (super.equals(o) && this.altitude.equals(ams.getAltitudeMilestones()) && this.reverseA.equals(rms.getReverseAltitudeMilestones()));
    }}

