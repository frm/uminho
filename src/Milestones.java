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
public class Milestones {
    private HashMap<Long,Integer> caloriesMS;

    //constructors
    public Milestones()
    {this.caloriesMS = new HashMap();}
    
    public Milestones(HashMap<Long,Integer> cms)
    {this.caloriesMS = cloneMilestones(cms);}
    
    public Milestones(Milestones m)
    {this.caloriesMS = m.getMilestones();}
    
    //getters & setters
    public HashMap<Long,Integer> getMilestones()
    {return cloneMilestones(this.caloriesMS);}
    
    public void setCaloriesMS(HashMap<Long, Integer> caloriesMS)
    {this.caloriesMS = cloneMilestones(caloriesMS);}
    
    //methods
    public HashMap<Long, Integer> cloneMilestones(HashMap<Long,Integer> m) {
        HashMap<Long,Integer> aux = new HashMap<Long,Integer>();
        for(Map.Entry<Long,Integer> cms: m.entrySet())
            aux.put(cms.getKey(), cms.getValue());
        return aux;
    }
    
    public void populateMilestones(){
        this.caloriesMS.put((long)10,-1);
        this.caloriesMS.put((long)30,-1);
        this.caloriesMS.put((long)60,-1);
        this.caloriesMS.put((long)120,-1);
        this.caloriesMS.put((long)180,-1);
    }
    
    public void addData(Activity act){
        long actDuration = act.getDuration();
        double actCalories = act.getCalories();
        
        for(Map.Entry<Long,Integer> pair : caloriesMS.entrySet()){
            if(actDuration >= pair.getKey()){
                actCalories = (actCalories*(pair.getKey()))/actDuration;
                
                if(actCalories > pair.getValue())
                    caloriesMS.put(pair.getKey(),(int)actCalories);
            }
            else break;
        }
    }
    //essentials
    public String toString(){
        return this.caloriesMS.toString();
    }
    
    public boolean equals(Object o){
        if (this == o) return true;
        
        if (o == null || this.getClass() != o.getClass()) return false;
        
        Milestones cms = (Milestones) o;
        
        return this.caloriesMS.equals(cms.getMilestones());
    }

    public Milestones clone() 
    {return new Milestones(this);}
}
