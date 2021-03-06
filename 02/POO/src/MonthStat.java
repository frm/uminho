
import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author joaorodrigues
 */
public class MonthStat implements Serializable{
    private HashMap<String, StatEntry> stats;
    
    /**
     *
     */
    public MonthStat(){
        this.stats = new HashMap<String, StatEntry>();
    }
    
    /**
     *
     * @param act
     */
    public MonthStat(Activity act){
        this.stats = new HashMap<String, StatEntry>();
        this.addStat(act);
    }
    
    /**
     *
     * @param stats
     */
    public MonthStat(HashMap<String, StatEntry> stats){
        this.stats = cloneStats(stats);
    }
    
    /**
     *
     * @param ms
     */
    public MonthStat(MonthStat ms){
        this.stats = ms.getStats();
    }
    
    /**
     *
     * @return
     */
    public HashMap<String, StatEntry> getStats(){
        return cloneStats(this.stats);
    }
    
    /**
     *
     */
    public void setStats(){
        this.stats = cloneStats(this.stats);
    }
    
    /**
     *
     * @param act
     */
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
     * @param act
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
    
    /**
     *
     * @param act
     * @return
     */
    public boolean removeStat(Activity act){
        StatEntry aux;
        aux = this.stats.get(act.getName());
        if(aux == null) return false;
        aux.removeActivityStat(act);
        this.stats.put(act.getName(), aux);
        return true;
    }
    
    /**
     *
     * @param act
     * @return
     */
    public long getTotalDuration(String act){
        return stats.get(act).getTotalDuration();
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
        StringBuilder result = new StringBuilder();
        for( Map.Entry<String, StatEntry> pair : this.stats.entrySet()){
            result.append("\n###" + pair.getKey() + "###\n");
            result.append( pair.getValue() );
        }
        
        return result.toString();
    }
    
    private HashMap<String, StatEntry> cloneStats(HashMap<String, StatEntry> s){
        HashMap<String, StatEntry> result = new HashMap<String, StatEntry>();
        
        for(StatEntry se: s.values()){
            result.put(se.getName(), se.clone());
        }
        return result;
    }
}
