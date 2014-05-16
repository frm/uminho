
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
    private HashMap<String, Activity> activities;
    
    public Stats(){
        this.activities = new HashMap<String, Activity>();
    }
    
    public Stats(HashMap<String, Activity> activities){
        this.activities = new HashMap<String, Activity>();
        for(Activity act: activities.values()){
            this.activities.put(act.getName(), act);
        }
    }
    
    public Stats(Stats stats){
        this.activities = new HashMap<String, Activity>();
        for(Activity act: stats.getActivities().values())
            this.activities.put(act.getName(), act);
    }
    
    public Map<String,Activity> getActivities(){
        HashMap<String,Activity> aux = new HashMap<String, Activity>();
        for(Activity act: this.activities.values())
            aux.put(act.getName(), act);
        return aux;
    }
    
    public void setActivities(HashMap<String,Activity> activities){
        HashMap<String,Activity> aux = new HashMap<String, Activity>();
        
        for(Activity act: activities.values())
            aux.put(act.getName(), act);
        
        this.activities = aux;
    }
    
    /**Get the activities the user has practiced at least once
     *
     * @return HashSet with the names of the activities he has practiced at least once.
     */
    public HashSet<String> getMyActivities(){
        HashSet<String> result = new HashSet<String>();

        for(Activity act: this.activities.values() ){
            result.add( act.getName() );
        }
        
        return result;
    }
    
    public void addActivity(Activity act){
        if(this.activities.containsKey(act.getName())) 
            updateStats(act);
        else 
            this.activities.put(act.getName(), act);
    }
    
    
    /**Update the statistics for an existing activity
     *
     */
    public void updateStats(Activity act){
        Activity aux = (this.activities).get( act.getName() );
        
        aux.setCalories( aux.getCalories() + act.getCalories() );

        long total = aux.getDuration().getTimeInMillis() + act.getDuration().getTimeInMillis();
        
        GregorianCalendar duration = new GregorianCalendar();
        duration.setTimeInMillis(total);
        
        aux.setDuration(duration);
        
        if( aux instanceof DistanceActivity){
            DistanceActivity newAux = (DistanceActivity) aux;
            DistanceActivity newAct = (DistanceActivity) act;
            newAux.setDistance( newAct.getDistance() + newAux.getDistance() );
        }
        
        this.activities.put(act.getName(), aux);
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
        
        return this.activities.equals(stats.getActivities() );
    }
    
    public String toString(){
        return (this.activities.toString() );
    }    
    
}
