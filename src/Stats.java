
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author joaorodrigues
 */
public class Stats {
    private HashMap<String, StatEntry> stats;
    
    public Stats(){
        this.stats = new HashMap<String, StatEntry>();
    }
    
    public Stats(HashMap<String, StatEntry> stats){
        this.stats = new HashMap<String, StatEntry>();
        for(StatEntry stat: stats.values()){
            this.stats.put(stat.getName(), stat);
        }
    }
    
    public Stats(Stats stats){
        this.stats = new HashMap<String, StatEntry>();
        for(StatEntry stat: stats.getStats().values())
            this.stats.put(stat.getName(), stat);
    }
    
    public Map<String,StatEntry> getstats(){
        HashMap<String,StatEntry> aux = new HashMap<String, StatEntry>();
        for(StatEntry stat: this.stats.values())
            aux.put(stat.getName(), stat);
        return aux;
    }
    
    public void setstats(HashMap<String,StatEntry> stats){
        HashMap<String,StatEntry> aux = new HashMap<String, StatEntry>();
        
        for(StatEntry stat: stats.values())
            aux.put(stat.getName(), stat);
        
        this.stats = aux;
    }
    
    /**Get the stats the user has practiced at least once
     *
     * @return HashSet with the names of the stats he has practiced at least once.
     */
    public HashSet<String> getSports(){
        HashSet<String> result = new HashSet<String>();

        for(StatEntry stat: this.stats.values() ){
            result.add( stat.getName() );
        }
        
        return result;
    }
    
    public void addStat(Activity act){
        if(this.stats.containsKey(act.getName())) 
            updateStats(act);
        else {
            StatEntry stat = newStatFromActivity(act);
            this.stats.put(act.getName(), stat );
        }
    }
    
    private StatEntry newStatFromActivity(Activity act){
        if( act instanceof AltitudeActivity){
            AltitudeActivity altAct = (AltitudeActivity) act;
            return new AltitudeStatEntry(altAct);
        }
        else if(act instanceof DistanceActivity){
            DistanceActivity disAct = (DistanceActivity) act;
            return new DistanceStatEntry(disAct);
        }
        
        else 
            return new StatEntry(act ); 
            
    }

    public HashMap<String, StatEntry> getStats() {
        HashMap<String,StatEntry> result = new HashMap<String, StatEntry>();
        for(StatEntry stat: this.stats.values()){
            result.put(stat.getName(), stat);
        }
        
        
        return result;
    }

    public void setStats(HashMap<String, StatEntry> stats) {
        this.stats = new HashMap<String, StatEntry>();
        for(StatEntry stat: stats.values()){
            this.stats.put(stat.getName(), stat);
        }
    }
    
    
    /**Update the statistics for an existing StatEntry
     *
     */
    public void updateStats(Activity act){
        StatEntry stat = stats.get(act.getName());     
        stat.updateStat(act);
        
    }
    
    public StatEntry getActivityStat(String name){
        return this.stats.get(name);
    }
    
    public Stats clone(){
        Stats stats = new Stats();
        
        try {
           stats = new Stats(this);
        } catch (NullPointerException e) {
            System.out.println("No stats yet");
            throw new NullPointerException( e.getMessage() );
        }
        
        return stats;
    }
    
    public boolean equals(Object o){
        if( this == o) return true;
        
        if( o == null || this.getClass() != o.getClass() ) return false;
        
        Stats stats = (Stats) o;
        
        return this.stats.equals(stats.getstats() );
    }
    
    public String toString(){
        return (this.stats.toString() );
    }    
    
}
