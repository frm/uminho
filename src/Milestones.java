import java.io.Serializable;
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
public class Milestones implements Serializable{
    private HashMap<Long,Integer> calories;

    //constructors
    public Milestones(){
        this.calories = new HashMap();
        populateMilestones();
     }

    public Milestones(HashMap<Long,Integer> cms) {
        this.calories = cloneMilestones(cms);
    }

    public Milestones(Milestones m)
    {this.calories = m.getMilestones();}

    //getters & setters
    public HashMap<Long,Integer> getMilestones()
    {return cloneMilestones(this.calories);}

    public void setCalories(HashMap<Long, Integer> calories)
    {this.calories = cloneMilestones(calories);}

    //methods
    public HashMap<Long, Integer> cloneMilestones(HashMap<Long,Integer> m) {
        HashMap<Long,Integer> aux = new HashMap<Long,Integer>();
        for(Map.Entry<Long,Integer> cms: m.entrySet())
            aux.put(cms.getKey(), cms.getValue());
        return aux;
    }

    public void populateMilestones(){
        this.calories.put(10L,-1);
        this.calories.put(30L,-1);
        this.calories.put(60L,-1);
        this.calories.put(120L,-1);
        this.calories.put(180L,-1);
    }

    public void addData(Activity act){
        long actDuration = act.getDuration();
        int actCalories = act.getCalories();

        for(Map.Entry<Long,Integer> pair : calories.entrySet()) {

            if( actDuration >= pair.getKey() ) {
                int aux = (int) ruleOfThree(actDuration/60000L, (long) actCalories, pair.getKey());

                if(aux > pair.getValue())
                    calories.put(pair.getKey(), aux);
            }
        }
    }

    public static long ruleOfThree(long l1, long l2, long r1){
        return (l2 * r1)/l1;
    }

    //essentials
    public String toString(){
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

            result.append(pair.getValue());
            result.append(" kCal\n");

        }
        return result.toString();
    }

    public boolean equals(Object o){
        if (this == o) return true;

        if (o == null || this.getClass() != o.getClass()) return false;

        Milestones cms = (Milestones) o;

        return this.calories.equals(cms.getMilestones());
    }

    public Milestones clone() {
        return new Milestones(this);
    }
}
