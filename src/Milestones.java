import java.io.Serializable;
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
public class Milestones implements Serializable{
    private TreeMap<Long,Integer> calories;
    private TreeMap<Integer,Long> reverseC;

    //constructors
    public Milestones(){
        populateMilestones();
     }

    public Milestones(TreeMap<Long,Integer> cms, TreeMap<Integer,Long> rC) {
        this.calories = cloneMilestones(cms);
        this.reverseC = cloneReverseMilestones(rC);
    }

    public Milestones(Milestones m){
        this.calories = m.getMilestones();
        this.reverseC = m.getReverseMilestones();
    }

    //getters & setters
    public TreeMap<Long,Integer> getMilestones()
    {return cloneMilestones(this.calories);}
    
    public TreeMap<Integer,Long> getReverseMilestones()
    {return cloneReverseMilestones(this.reverseC);}

    public void setCalories(TreeMap<Long, Integer> calories)
    {this.calories = cloneMilestones(calories);}
    
    public void setReverseCalories(TreeMap<Integer,Long> rC)
    {this.reverseC = cloneReverseMilestones(rC);}

    //methods
    public TreeMap<Long, Integer> cloneMilestones(TreeMap<Long,Integer> m) {
        TreeMap<Long,Integer> aux = new TreeMap<Long,Integer>();
        for(Map.Entry<Long,Integer> cms: m.entrySet())
            aux.put(cms.getKey(), cms.getValue());
        return aux;
    }

    public TreeMap<Integer,Long> cloneReverseMilestones(TreeMap<Integer,Long> r) {
        TreeMap<Integer,Long> aux = new TreeMap<Integer,Long>();
        for(Map.Entry<Integer,Long> rms: r.entrySet())
            aux.put(rms.getKey(), rms.getValue());
        return aux;
    }
    
    public void populateMilestones(){
        this.calories = new TreeMap<Long, Integer>();
        this.reverseC = new TreeMap<Integer, Long>();
        this.calories.put(10L,-1);
        this.calories.put(30L,-1);
        this.calories.put(60L,-1);
        this.calories.put(120L,-1);
        this.calories.put(180L,-1);
        this.reverseC.put(50,-1L);
        this.reverseC.put(100,-1L);
        this.reverseC.put(250,-1L);
        this.reverseC.put(500,-1L);
    }

    public void addData(Activity act){
        int actMinDuration = (int) (act.getDuration()/60000L);
        long actDuration = act.getDuration();
        int actCalories = act.getCalories();

        for(Map.Entry<Long,Integer> pair : calories.entrySet()) {

            if( actMinDuration >= pair.getKey() ) {
                int aux = (int) ruleOfThree(actMinDuration, (long) actCalories, pair.getKey());

                if(aux > pair.getValue())
                    calories.put(pair.getKey(), aux);
            }
            else break;
        }
        
        
        for(Map.Entry<Integer,Long> pair : reverseC.entrySet()) {

            if( actCalories >= pair.getKey() ) {
                int aux = (int) ruleOfThree( (long) actCalories, actDuration/60000L, pair.getKey());

                if(aux > pair.getValue())
                    reverseC.put(pair.getKey(), (long)aux);
            }
            else break;
        }
    }

    public static long ruleOfThree(long l1, long l2, long r1){
        return (l2 * r1)/l1;
    }

    //essentials
    public String firsttoString(){
        StringBuilder result = new StringBuilder();

        for(Map.Entry<Long,Integer> pair: this.calories.entrySet()){

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
                result.append(" kCal\n");
            }
        }
        return result.toString();
    }
    
    public String secondtoString(){
        StringBuilder result = new StringBuilder();
        
        result.append("\n");
        for(Map.Entry<Integer,Long> pair: this.reverseC.entrySet()){
            
            result.append(pair.getKey());
            result.append(" kCal: ");
            
            if(pair.getValue() == -1)
                result.append(" No info\n");
            else{
                result.append(pair.getValue());
                result.append(" minutes\n");
            }
        }
        
        return result.toString();
    }

    public String toString(){
        return (firsttoString() + secondtoString());
    }
    
    public boolean equals(Object o){
        if (this == o) return true;

        if (o == null || this.getClass() != o.getClass()) return false;

        Milestones cms = (Milestones) o;
        Milestones rC = (Milestones) o;
        
        return (this.calories.equals(cms.getMilestones()) && this.reverseC.equals(rC.getReverseMilestones()));
    }

    public Milestones clone() {
        return new Milestones(this);
    }
}
