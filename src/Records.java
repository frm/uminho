
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.GregorianCalendar;


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
        this.activities = new HashMap<String, Activity>();
        this.activities.putAll(activities);
    }
    
    public Records(Records rec){
        this.activities = new HashMap<String, Activity>();
        this.activities.putAll(rec.getActivities());
    }
    
    public Map<String,Activity> getActivities(){
        HashMap<String,Activity> aux = new HashMap<String, Activity>();
        aux.putAll(this.activities);
        return aux;
    }
    
    public void setActivities(HashMap<String,Activity> activities){
        HashMap<String,Activity> aux = new HashMap<String, Activity>();
        
        aux.putAll(this.activities);
        
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
        Records rec = new Records();
        
        try {
           rec = new Records(this);
        } catch (NullPointerException e) {
            System.out.println("No records yet");
            throw new NullPointerException( e.getMessage() );
        }
        
        return rec;
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
    