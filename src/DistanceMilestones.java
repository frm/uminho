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
public class DistanceMilestones extends Milestones{
    private TreeMap<Long, Integer> distance;
    private TreeMap<Integer,Long> reverseD;

    //constructors public
    public DistanceMilestones(){
        super();
    }

    public DistanceMilestones(TreeMap<Long,Integer> cms, TreeMap<Integer,Long> rC, TreeMap<Long,Integer> dms, TreeMap<Integer,Long> rD){
        super(cms,rC);
        this.distance = cloneDistanceMilestones(dms);
    }

    public DistanceMilestones(DistanceMilestones dms){
        super(dms);
        this.distance = dms.getDistanceMilestones();
        this.reverseD = dms.getReverseDistanceMilestones();
    }

    //setters & getters
    public TreeMap<Long,Integer> getDistanceMilestones()
    {return cloneDistanceMilestones(this.distance);}

    public TreeMap<Integer,Long> getReverseDistanceMilestones()
    {return cloneReverseDistanceMilestones(this.reverseD);}

    public void setDistance(TreeMap<Long,Integer> distance)
    {this.distance = cloneDistanceMilestones(distance);}

    public void setReverseDistance(TreeMap<Integer,Long> rD)
    {this.reverseD = cloneReverseDistanceMilestones(rD);}

    //methods
    public TreeMap<Long,Integer> cloneDistanceMilestones(TreeMap<Long,Integer> dm) {
        TreeMap<Long,Integer> aux = new TreeMap<Long,Integer>();
        for(Map.Entry<Long,Integer> dms: dm.entrySet())
            aux.put(dms.getKey(), dms.getValue());
        return aux;
    }

    public TreeMap<Integer,Long> cloneReverseDistanceMilestones(TreeMap<Integer,Long> dm) {
        TreeMap<Integer,Long> aux = new TreeMap<Integer,Long>();
        for(Map.Entry<Integer,Long> dms: dm.entrySet())
            aux.put(dms.getKey(), dms.getValue());
        return aux;
    }

    public void populateMilestones(){
        super.populateMilestones();
        this.distance = new TreeMap<Long,Integer>();
        this.reverseD = new TreeMap<Integer,Long>();
        this.distance.put(10L,-1);
        this.distance.put(30L,-1);
        this.distance.put(60L,-1);
        this.distance.put(120L,-1);
        this.distance.put(180L,-1);
        this.reverseD.put(1000,-1L);
        this.reverseD.put(5000,-1L);
        this.reverseD.put(10000,-1L);
        this.reverseD.put(20000,-1L);
    }

    public void addData(DistanceActivity act){
        super.addData(act);
        int actMinDuration = (int) (act.getDuration()/60000L);
        long actDuration = act.getDuration();
        int actDistance = act.getDistance();

        for(Map.Entry<Long,Integer> pair : distance.entrySet()){
            if(actDuration >= pair.getKey()){
                long aux = ruleOfThree(actMinDuration, actDistance,  pair.getKey());

                if(aux > pair.getValue())
                    distance.put(pair.getKey(), (int)aux);
            }
            else break;
        }
        
        for(Map.Entry<Integer,Long> pair : reverseD.entrySet()){

            if(actDistance >= pair.getKey()){
                long aux = ruleOfThree((long) actDistance, actDuration/60000L, pair.getKey());

                if(aux > pair.getValue())
                    reverseD.put( (int)pair.getKey(), aux);
            }
            else break;
        }
    }
    //essentials
    public DistanceMilestones clone()
    {return new DistanceMilestones(this);}

    
    public String firsttoString(){
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
    
    public String secondtoString(){
        StringBuilder result = new StringBuilder();

        result.append("\n");
        for(Map.Entry<Integer,Long> pair: this.reverseD.entrySet()){
            result.append( (pair.getKey())/1000 );
            result.append( " km: ");

            if(pair.getValue() == -1)
                result.append(" No info\n");
            else
                result.append( StatEntry.formatMillis( pair.getValue() ));
        }

        return result.toString();
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();

        sb.append(firsttoString());
        sb.append(secondtoString());

        return sb.toString();
    }

    public boolean equals(Object o){
        if (this == o) return true;

        if (o == null || this.getClass() != o.getClass()) return false;

        DistanceMilestones dms = (DistanceMilestones) o;
        DistanceMilestones rD = (DistanceMilestones) o;

        return (this.distance.equals(dms.getDistanceMilestones()) && this.reverseD.equals(rD.getReverseDistanceMilestones()));
    }
}
