
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;


/**
 *
 * @author joaorodrigues
 */
public class ActivityInfo {
    private TreeSet<Activity> activityLog;
    private Stats stats;

    public ActivityInfo() {
        this.activityLog = new TreeSet<Activity>(new ActivityComparator());
        this.stats = new Stats();
    }
    
    public ActivityInfo(TreeSet<Activity> activityLog, Stats stats) {
        this.activityLog = cloneActivityLog(activityLog);
        this.stats = stats.clone();
    }
    
    public ActivityInfo(ActivityInfo info){
        this.activityLog = info.getActivityLog();
        this.stats = info.getStats();
    }

    public void setActivityLog(TreeSet<Activity> activityLog) {
        this.activityLog = cloneActivityLog(activityLog);
    }

    public void setStats(Stats stats) {
        this.stats = stats.clone();
    }

    public TreeSet<Activity> getActivityLog() {
        return cloneActivityLog(this.activityLog);
    }

    public Stats getStats() {
        return this.stats.clone();
    }
   
   
    
    private TreeSet<Activity> cloneActivityLog(TreeSet<Activity> aL){
        TreeSet<Activity> result= new TreeSet<Activity>();
        
        for(Activity act: aL){
            result.add(act);
        }
        return result;
    }

    /**Get the 10 more recent activities by an user
     *
     * @return Array with the 10 most recent activities
     */
    public ArrayList<Activity> getMostRecent() {
        int count = 0;
        ArrayList<Activity> result = new ArrayList<Activity>();
        Iterator<Activity> it= this.activityLog.iterator();
            
        while (it.hasNext() && count < 10)
            result.add(it.next().clone());
        return result;
    }
    
    public boolean addActivity(Activity act){
        stats.addStat(act);
        return activityLog.add(act);
    }
    
    public boolean removeActivity(Activity act){
        boolean actRemoval = this.activityLog.remove(act);
        boolean statRemoval = this.stats.removeActivityStat(act);
        return (actRemoval && statRemoval);
    }
    
    public String statsOverview(){
        return this.stats.toString();
    }
    
    public String showAnnualStats(int year){
        return this.stats.showAnnualStats(year);
    }
    
    public String showMonthlyStats(int year, int month){
        return this.stats.showMonthlyStats(year, month);
    }
    
    public ActivityInfo clone(){
        return new ActivityInfo(this);
    }
    
    public boolean equals(Object o){
        if( this == o) return true;
        
        if( o == null || this.getClass() != o.getClass() ) return false;
        
        ActivityInfo info = (ActivityInfo) o;
        
        return ( this.stats.equals(info.getStats()) && this.activityLog.equals(info.getActivityLog() ));
    }
    
    public String toString(){
        StringBuilder result = new StringBuilder();
        result.append("\nACTIVITY LOG: ");
        result.append(this.activityLog);
        result.append("\nSTATS: ");
        result.append(this.stats);
        
        return result.toString();
    }
    
    
}
