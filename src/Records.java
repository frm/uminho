
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.GregorianCalendar;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author joaorodrigues
 */
public class Records {
    private HashMap<String, Activity> activities;
    
    public Records(){
        this.activities = new HashMap<String, Activity>();
    }
    
    public Records(HashMap<String, Activity> activities){
        HashMap<String,Activity> aux = new HashMap<String, Activity>();
        
        for(Activity act: activities.values()){
            aux.put(act.getName() , act.clone());
        }
        
        this.activities = aux;
    }
    
    public Records(Records rec){
        HashMap<String,Activity> aux = new HashMap<String, Activity>();
        
        for(Activity act: rec.getActivities().values()){
            aux.put(act.getName() , act.clone());
        }
        
        this.activities = aux;
    }
    
    public Map<String,Activity> getActivities(){
        HashMap<String,Activity> aux = new HashMap<String, Activity>();
        
        for(Activity act: this.activities.values()){
            aux.put(act.getName() , act.clone());
        }
        
        return aux;
    }
    
    public void setActivities(HashMap<String,Activity> activities){
        HashMap<String,Activity> aux = new HashMap<String, Activity>();
        
        for(Activity act: activities.values()){
            aux.put(act.getName() , act.clone());
        }
        
        this.activities = aux;
    }
    
    
    /**Get the activities the user has practiced at least once
     *
     * @return HashSet with the names of the activities he has practiced at least once
     */
    public HashSet<String> getMyActivities(){
        HashSet<String> result = new HashSet<String>();

        for(Activity act: this.activities.values() ){
            result.add( act.getName() );
        }
        
        return result;
    }
    
    public void addActivity(Activity act){
        if(this.activities.containsValue(act)) 
            updateRecords(act);
        else 
            this.activities.put(act.getName(), act);
    }
    
    public void updateRecords(Activity act){
        Activity aux = (this.activities).get( act.getName() );
        
        if( aux.getCalories() < act.getCalories())
            aux.setCalories( act.getCalories() );
        
        long total = aux.getDuration().getTimeInMillis() + act.getDuration().getTimeInMillis();
        
        GregorianCalendar duration = new GregorianCalendar();
        duration.setTimeInMillis(total);
        
        aux.setDuration(duration);
        
        this.activities.put(act.getName(), aux);
    }
    
    
    public Records clone(){
        return new Records(this);
    }
    
    public boolean equals(Object o){
        if( this == o) return true;
        
        if( o == null || this.getClass() != o.getClass() ) return false;
        
        Records rec = (Records) o;
        
        return this.activities.equals(rec.getActivities() );
    }
    
    public String toString(){
        return ("Records: \n" + this.activities.toString() );
    }
    
}
    