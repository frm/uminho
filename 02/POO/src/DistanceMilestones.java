
import java.util.Iterator;
import java.util.TreeMap;
import java.util.Map;


/**Milestones for an activity with distance
 *
 * @author tiago
 */
public class DistanceMilestones extends Milestones{
    private TreeMap<Long, Integer> distance;
    private TreeMap<Integer,Long> reverseD;

    //constructors public

    /**
     *
     */
        public DistanceMilestones(){
        super();
        this.populateMilestones();
    }

    /**
     *
     * @param cms
     * @param rC
     * @param dms
     * @param rD
     */
    public DistanceMilestones(TreeMap<Long,Integer> cms, TreeMap<Integer,Long> rC, TreeMap<Long,Integer> dms, TreeMap<Integer,Long> rD){
        super(cms,rC);
        this.distance = cloneDistanceMilestones(dms);
        this.reverseD = cloneReverseDistanceMilestones(rD);
    }
    
    /**Creates distance milestones from the information in an activity
     *
     * @param act activity that supplies the information
     */
    public DistanceMilestones(DistanceActivity act){
        super(act);
        this.populateMilestones();
        this.addData(act);
    }

    /**
     *
     * @param dms
     */
    public DistanceMilestones(DistanceMilestones dms){
        super(dms);
        this.distance = dms.getDistanceMilestones();
        this.reverseD = dms.getReverseDistanceMilestones();
    }

    //setters & getters

    /**
     *
     * @return
     */
        public TreeMap<Long,Integer> getDistanceMilestones()
    {return cloneDistanceMilestones(this.distance);}

    /**
     *
     * @return
     */
    public TreeMap<Integer,Long> getReverseDistanceMilestones()
    {return cloneReverseDistanceMilestones(this.reverseD);}

    /**
     *
     * @param distance
     */
    public void setDistance(TreeMap<Long,Integer> distance)
    {this.distance = cloneDistanceMilestones(distance);}

    /**
     *
     * @param rD
     */
    public void setReverseDistance(TreeMap<Integer,Long> rD)
    {this.reverseD = cloneReverseDistanceMilestones(rD);}

    //methods

    /**Alternative altitude clone, not shallow
     *
     * @param 
     * @return
     */
        public TreeMap<Long,Integer> cloneDistanceMilestones(TreeMap<Long,Integer> dm) {
        TreeMap<Long,Integer> aux = new TreeMap<Long,Integer>();
        for(Map.Entry<Long,Integer> dms: dm.entrySet())
            aux.put(dms.getKey(), dms.getValue());
        return aux;
    }

    /**Alternative reverseA clone, not shallow
     *
     * @param 
     * @return
     */
    public TreeMap<Integer,Long> cloneReverseDistanceMilestones(TreeMap<Integer,Long> dm) {
        TreeMap<Integer,Long> aux = new TreeMap<Integer,Long>();
        for(Map.Entry<Integer,Long> dms: dm.entrySet())
            aux.put(dms.getKey(), dms.getValue());
        return aux;
    }

    /**Creates default values for the milestones tables after initializing them
     *
     */
    public void populateMilestones(){
        super.populateMilestones();
        this.distance = new TreeMap<Long,Integer>();
        this.reverseD = new TreeMap<Integer,Long>();
        this.distance.put(10L,-1);
        this.distance.put(30L,-1);
        this.distance.put(60L,-1);
        this.distance.put(120L,-1);
        this.distance.put(180L,-1);
        this.reverseD.put(1000,Long.MAX_VALUE);
        this.reverseD.put(5000,Long.MAX_VALUE);
        this.reverseD.put(10000,Long.MAX_VALUE);
        this.reverseD.put(20000,Long.MAX_VALUE);
    }
    
    /**updates the distance milestones based on the information in a given activity
     *
     * @param act activity that will possibly change the milestones
     */
    private void addDistanceData(DistanceActivity act) {
        int actMinDuration = (int) (act.getDuration()/60000L);
        int actDistance = act.getDistance();
        
        Iterator it = distance.entrySet().iterator();
        boolean hasFinished = false;
        
        while( it.hasNext() && !hasFinished ) {
            Map.Entry<Long, Integer> pair = (Map.Entry<Long, Integer>)it.next();
            
            if ( actMinDuration >= pair.getKey() ) {
                int aux = (int) ruleOfThree(actMinDuration, (long)actDistance, pair.getKey() );
                if( aux > pair.getValue() )
                    distance.put(pair.getKey(), aux);
            }
            else hasFinished = true;
        }
    }
    
    /**updates the reverseD milestones based on the information in a given activity
     *
     * @param act activity that will possibly change the milestones
     */
    private void addTimeDistanceData(DistanceActivity act) {
        int actMinDuration = (int) (act.getDuration()/60000L);
        int actDistance = act.getDistance();
        
        Iterator it = reverseD.entrySet().iterator();
        boolean hasFinished = false;
        
        while( it.hasNext() && !hasFinished) {
            Map.Entry<Integer, Long> pair = (Map.Entry<Integer, Long>)it.next();
            
            if( actDistance >= pair.getKey() ) {
                int aux = (int) ruleOfThree( (long) actDistance, actMinDuration, pair.getKey() );
                if(aux < pair.getValue())
                    reverseD.put(pair.getKey(), (long)aux);
            }
            
            else hasFinished = false;
        }
    }

    /**Updates all the milestones based on the information in from a given activity
     *
     * @param act activity that will possibly change the milestones
     */
    public void addData(DistanceActivity act){
        super.addData(act);
        addDistanceData(act);
        addTimeDistanceData(act);
    }
    
    /**Gets the approximate time the user takes to complete 1 km
     *
     * @return
     */
    public long getKmTimeAprox(){
        long km = this.reverseD.get(1000);
        if(km!=Long.MAX_VALUE)
            return km;
        else
            return 0;
    }
    
    /**Gets the largest distance the user has completed
     *
     * @return
     */
    public int getMaxRecordDistance(){
        int maxRecordDistance = 0;
        for(Map.Entry<Long,Integer> pair: this.distance.entrySet()){
            if(pair.getValue() > 0)
                maxRecordDistance = pair.getValue();
        }
        return maxRecordDistance;
    }
    
    //essentials
    public DistanceMilestones clone()
    {return new DistanceMilestones(this);}

    /**toString for the distance variable
     *
     * @return
     */
    public String firsttoStringD(){
        StringBuilder result = new StringBuilder();

        for(Map.Entry<Long,Integer> pair: this.distance.entrySet()){

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
   
    /**toString for the reverseD variable
     *
     * @return
     */
    public String secondtoStringD(){
        StringBuilder result = new StringBuilder();

        result.append("\n");
        for(Map.Entry<Integer,Long> pair: this.reverseD.entrySet()){
            result.append( (pair.getKey())/1000);
            result.append( " km: ");

            if(pair.getValue() == Long.MAX_VALUE)
                result.append(" No info\n");
            else{
                result.append(pair.getValue());
                result.append(" minutes\n");
            }
                
        }

        return result.toString();
    }

    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder();
        
        sb.append(super.toString());
        sb.append("\n###Distance Milestones###\n");
        sb.append(firsttoStringD());
        sb.append(secondtoStringD());

        return sb.toString();
    }

    @Override
    public boolean equals(Object o){
        if (this == o) return true;

        if (o == null || this.getClass() != o.getClass()) return false;

        DistanceMilestones dms = (DistanceMilestones) o;
        DistanceMilestones rD = (DistanceMilestones) o;

        return ( super.equals(o) && this.distance.equals(dms.getDistanceMilestones()) && this.reverseD.equals(rD.getReverseDistanceMilestones()));
    }
}
