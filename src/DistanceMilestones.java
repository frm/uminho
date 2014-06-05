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
    private TreeMap<Integer,Long> distance;
    private TreeMap<Long, Integer> reverseD;
    
    //constructors public
    public DistanceMilestones(){
        super();
    }

    public DistanceMilestones(TreeMap<Long,Integer> cms, TreeMap<Integer,Long> rC, TreeMap<Integer,Long> dms, TreeMap<Long,Integer> rD){
        super(cms,rC);
        this.distance = cloneDistanceMilestones(dms);
    }

    public DistanceMilestones(DistanceMilestones dms){
        super(dms);
        this.distance = dms.getDistanceMilestones();
        this.reverseD = dms.getReverseDistanceMilestones();
    }

    //setters & getters
    public TreeMap<Integer,Long> getDistanceMilestones()
    {return cloneDistanceMilestones(this.distance);}

    public TreeMap<Long,Integer> getReverseDistanceMilestones()
    {return cloneReverseDistanceMilestones(this.reverseD);}
    
    public void setDistance(TreeMap<Integer,Long> distance)
    {this.distance = cloneDistanceMilestones(distance);}

    public void setReverseDistance(TreeMap<Long,Integer> rD)
    {this.reverseD = cloneReverseDistanceMilestones(rD);}
    
    //methods
    public TreeMap<Integer,Long> cloneDistanceMilestones(TreeMap<Integer,Long> dm) {
        TreeMap<Integer,Long> aux = new TreeMap<Integer,Long>();
        for(Map.Entry<Integer,Long> dms: dm.entrySet())
            aux.put(dms.getKey(), dms.getValue());
        return aux;
    }

    public TreeMap<Long,Integer> cloneReverseDistanceMilestones(TreeMap<Long,Integer> dm) {
        TreeMap<Long,Integer> aux = new TreeMap<Long,Integer>();
        for(Map.Entry<Long,Integer> dms: dm.entrySet())
            aux.put(dms.getKey(), dms.getValue());
        return aux;
    }
    
    public void populateMilestones(){
        super.populateMilestones();
        this.distance = new TreeMap<Integer,Long>();
        this.reverseD = new TreeMap<Long,Integer>();
        this.distance.put(1000,-1L);
        this.distance.put(5000,-1L);
        this.distance.put(10000,-1L);
        this.distance.put(20000,-1L);
        this.reverseD.put(10L,-1);
        this.reverseD.put(30L,-1);
        this.reverseD.put(60L,-1);
        this.reverseD.put(120L,-1);
        this.reverseD.put(180L,-1);
    }

    public void addData(DistanceActivity act){
        super.addData(act);
        int actMinDuration = (int) (act.getDuration()/60000L);
        long actDuration = act.getDuration();
        int actDistance = act.getDistance();

        for(Map.Entry<Integer,Long> pair : distance.entrySet()){
            
            if(actDistance >= pair.getKey()){
                long aux = ruleOfThree(actMinDuration, (long) actDistance, pair.getKey());

                if(aux > pair.getValue())
                    distance.put( (int)pair.getKey(), aux);
            }
            else break;
        }
        
        for(Map.Entry<Long,Integer> pair : reverseD.entrySet()){
            if(actDuration >= pair.getKey()){
                long aux = ruleOfThree((long) actDistance, actDuration/60000L,  pair.getKey());

                if(aux > pair.getValue())
                    reverseD.put(pair.getKey(), (int)aux);
            }
            else break;
        }
    }
    //essentials
    public DistanceMilestones clone()
    {return new DistanceMilestones(this);}

    public String firsttoString(){
        StringBuilder result = new StringBuilder();

        for(Map.Entry<Integer,Long> pair: this.distance.entrySet()){
            result.append( (pair.getKey())/1000 );
            result.append( " km: ");
            
            if(pair.getValue() == -1)
                result.append(" No info\n");
            else
                result.append( StatEntry.formatMillis( pair.getValue() ));
        }
        
        return result.toString();
    }
    
    public String secondtoString(){
        StringBuilder result = new StringBuilder();

        for(Map.Entry<Long,Integer> pair: this.reverseD.entrySet()){
            result.append( StatEntry.formatMillis( pair.getKey() ));
            result.append( " : ");
            
            if(pair.getValue() == -1)
                result.append(" No info\n");
            else
                result.append( (pair.getValue())/1000 );
        }
        
        return result.toString();
    }
    
    public String toString(){
        StringBuilder sb = new StringBuilder();
        
        sb.append(super.toString());
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
