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
    private HashMap<Long,Integer> caloriesMS;

    //constructors
    public Milestones(){
        this.caloriesMS = new HashMap();
        populateMilestones();
     }
    
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
        this.caloriesMS.put(10L,-1);
        this.caloriesMS.put(30L,-1);
        this.caloriesMS.put(60L,-1);
        this.caloriesMS.put(120L,-1);
        this.caloriesMS.put(180L,-1);
    }
    
    public void addData(Activity act){
        long actDuration = act.getDuration();
        int actCalories = act.getCalories();
        
        for(Map.Entry<Long,Integer> pair : caloriesMS.entrySet()) {
            
            if( actDuration >= pair.getKey() ) {
                int aux = (int) threeSimple(actDuration/60000L, (long) actCalories, pair.getKey());
                System.out.println(aux);
                
                if(aux > pair.getValue())
                    caloriesMS.put(pair.getKey(), aux);
            }
        }
    }
    
    public static long threeSimple(long l1, long l2, long r1){
        return (l2 * r1)/l1;
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
