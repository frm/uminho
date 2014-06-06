
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;


/**
 *
 * @author joaorodrigues
 */
public class ActivityInfo implements Serializable{
    private TreeSet<Activity> activityLog;
    private Stats stats;
    private Records records;

    public ActivityInfo() {
        this.activityLog = new TreeSet<Activity>(new ActivityComparator());
        this.stats = new Stats();
        this.records = new Records();
    }
    
    public ActivityInfo(TreeSet<Activity> activityLog, Stats stats, Records records) {
        this.activityLog = cloneActivityLog(activityLog);
        this.stats = stats.clone();
        this.records = records.clone();
    }
    
    public ActivityInfo(ActivityInfo info){
        this.activityLog = info.getActivityLog();
        this.stats = info.getStats();
        this.records = info.getRecords();
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
    
    public Records getRecords() {
        return this.records.clone();
    }
   
   public Milestones getMilestones(String s){
       return this.records.getRecordEntry(s);
   }
    
    private TreeSet<Activity> cloneActivityLog(TreeSet<Activity> aL){
        TreeSet<Activity> result= new TreeSet<Activity>(new ActivityComparator());
        
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
    
    public boolean isAliasedActivity(Activity act) {
        Iterator<Activity> it = this.activityLog.iterator();
        boolean alias = false;
        
        while( it.hasNext() && !alias )
            alias = act.aliasOf( it.next() );
        
        return alias;
    }
    
    public boolean addActivity(Activity act) {
        boolean validTime = !isAliasedActivity(act) && act.hasMinimumTime();
        
        if(validTime) {
            stats.addStat(act);
            activityLog.add(act);
            records.addRecord(act);
        }
        
        return validTime;        
    }
    
    public boolean removeActivity(Activity act){
        boolean actRemoval = this.activityLog.remove(act);
        boolean statRemoval = this.stats.removeActivityStat(act);
        this.restoreRecords(act);
        return (actRemoval && statRemoval);
    }
    
    public String statsOverview() throws StatsNotAvailable{
        String result = this.stats.toString();
        if(result.length() == 0)
            throw(new StatsNotAvailable() );
        
        return this.stats.toString();
    }
    
    public String showAnnualStats(int year) throws StatsNotAvailable{
        return this.stats.showAnnualStats(year);
    }
    
    public String showMonthlyStats(int year, int month) throws StatsNotAvailable{
        return this.stats.showMonthlyStats(year, month);
    }
    
    public ArrayList<String> getPracticedActivities(){
        ArrayList<String> result = new ArrayList<String>();
        for( Activity a: activityLog){
            if(!result.contains( a.getName() ) )
                    result.add(a.getName());
        }
        return result;
    }
    
    public void restoreRecords(Activity act){
        this.records.removeRecord( act.getName() );
        
        for(Activity a: this.activityLog){
            if( act.getName().equals( a.getName()))
                this.records.addRecord(a);
        }
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
        result.append("\nRECORDS: \n");
        result.append(this.records);
        
        return result.toString();
    }
    
    
}
