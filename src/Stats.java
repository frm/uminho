
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
        for(StatEntry act: stats.values()){
            this.stats.put(act.getName(), act);
        }
    }
    
    public Stats(Stats stats){
        this.stats = new HashMap<String, StatEntry>();
        for(StatEntry act: stats.getstats().values())
            this.stats.put(act.getName(), act);
    }
    
    public Map<String,StatEntry> getstats(){
        HashMap<String,StatEntry> aux = new HashMap<String, StatEntry>();
        for(StatEntry act: this.stats.values())
            aux.put(act.getName(), act);
        return aux;
    }
    
    public void setstats(HashMap<String,StatEntry> stats){
        HashMap<String,StatEntry> aux = new HashMap<String, StatEntry>();
        
        for(StatEntry act: stats.values())
            aux.put(act.getName(), act);
        
        this.stats = aux;
    }
    
    /**Get the stats the user has practiced at least once
     *
     * @return HashSet with the names of the stats he has practiced at least once.
     */
    public HashSet<String> getMystats(){
        HashSet<String> result = new HashSet<String>();

        for(StatEntry act: this.stats.values() ){
            result.add( act.getName() );
        }
        
        return result;
    }
    
    public void addActivity(Activity act){
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
    
    
    /**Update the statistics for an existing StatEntry
     *
     */
    public void updateStats(Activity act){
        StatEntry stat = stats.get(act.getName());     
        stat.updateStat(act);
        
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
