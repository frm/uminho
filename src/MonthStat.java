
import java.util.HashMap;
import java.util.TreeSet;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author joaorodrigues
 */
public class MonthStat {
    private HashMap<String, StatEntry> stats;
    
    public MonthStat(){
        this.stats = new HashMap<String, StatEntry>();
    }
    
    public MonthStat(HashMap<String, StatEntry> stats){
        this.stats = cloneStats(stats);
    }
    
    public MonthStat(MonthStat ms){
        this.stats = ms.getStats();
    }
    
    public HashMap<String, StatEntry> getStats(){
        return cloneStats(this.stats);
    }
    
    public void setStats(){
        this.stats = cloneStats(this.stats);
    }
    
    public void addStat(Activity act){
        if(this.stats.containsKey(act.getClass().getSimpleName())) 
            updateStats(act);
        else {
            StatEntry stat = newStat(act);
            this.stats.put( act.getClass().getSimpleName(), stat );
        }
    }
    
     /**Update the statistics for an existing StatEntry
     *
     */
    public void updateStats(Activity act){
        StatEntry stat = stats.get(act.getClass().getSimpleName() );     
        stat.updateStat(act);
        
    }
    
    private StatEntry newStat(Activity act){
        if( act instanceof AltitudeActivity){
            AltitudeActivity altAct = (AltitudeActivity) act;
            return new AltitudeStatEntry(altAct);
        }
        else if(act instanceof DistanceActivity){
            DistanceActivity disAct = (DistanceActivity) act;
            return new DistanceStatEntry(disAct);
        }
        
        else 
            return new StatEntry(act); 
    }
    
    public boolean removeStat(Activity act){
        StatEntry aux;
        aux = this.stats.get(act.getName());
        if(aux == null) return false;
        aux.removeActivityStat(act);
        this.stats.put(act.getName(), aux);
        return true;
    }
    
    
    public MonthStat clone(){
        return new MonthStat(this);
    }
    
    public boolean equals(Object o){
        if( this == o) return true;
        
        if( o == null || this.getClass() != o.getClass() ) return false;
        
        MonthStat mStat = (MonthStat) o;
        
        return this.stats.equals(mStat.getStats());
    }
    
    public String toString(){
        return this.stats.toString();
    }
    
    private HashMap<String, StatEntry> cloneStats(HashMap<String, StatEntry> s){
        HashMap<String, StatEntry> result = new HashMap<String, StatEntry>();
        
        for(StatEntry se: s.values()){
            result.put(se.getName(), se.clone());
        }
        return result;
    }
}
