
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.TreeSet;


/**Info related to the user's activities
 *
 * @author joaorodrigues
 */
public class ActivityInfo implements Serializable{
    private TreeSet<Activity> activityLog;
    private Stats stats;
    private Records records;

    /**Empty constructor
     *
     */
    public ActivityInfo() {
        this.activityLog = new TreeSet<Activity>(new ActivityComparator());
        this.stats = new Stats();
        this.records = new Records();
    }
    
    /**Parameter constructor
     *
     * @param activityLog
     * @param stats
     * @param records
     */
    public ActivityInfo(TreeSet<Activity> activityLog, Stats stats, Records records) {
        this.activityLog = cloneActivityLog(activityLog);
        this.stats = stats.clone();
        this.records = records.clone();
    }
    
    /**Copy constructor
     *
     * @param info
     */
    public ActivityInfo(ActivityInfo info){
        this.activityLog = info.getActivityLog();
        this.stats = info.getStats();
        this.records = info.getRecords();
    }

    /**activityLog setter
     *
     * @param activityLog
     */
    public void setActivityLog(TreeSet<Activity> activityLog) {
        this.activityLog = cloneActivityLog(activityLog);
    }

    /**stats setter
     *
     * @param stats
     */
    public void setStats(Stats stats) {
        this.stats = stats.clone();
    }

    /**activityLog getter
     *
     * @return
     */
    public TreeSet<Activity> getActivityLog() {
        return cloneActivityLog(this.activityLog);
    }

    /**stats getter
     *
     * @return
     */
    public Stats getStats() {
        return this.stats.clone();
    }
    
    /**records getter
     *
     * @return
     */
    public Records getRecords() {
        return this.records.clone();
    }
   
    /**milestones getter
     *
     * @param s
     * @return
     */
    public Milestones getMilestones(String s){
       return this.records.getRecordEntry(s);
   }
    
    /**clones a treeSet, and is not a shallow clone, unlike the default clone
     *
     * @param aL tree to copy
     * @return copied treeset
     */
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
    
    /**Checks if an activity interferes with any performed activities' time frame
     *
     * @param act
     * @return true if aliased
     */
    public boolean isAliasedActivity(Activity act) {
        Iterator<Activity> it = this.activityLog.iterator();
        boolean alias = false;
        
        while( it.hasNext() && !alias )
            alias = act.aliasOf( it.next() );
        
        return alias;
    }
    
    /**Adds an activity to the activityLog
     *
     * @param act
     * @return false if it was inserted, false if there were problems inserting (aliased, or the duration is too short)
     */
    public boolean addActivity(Activity act) {
        boolean validTime = !isAliasedActivity(act) && act.hasMinimumTime();
        
        if(validTime) {
            activityLog.add(act);
            stats.addStat(act);
            records.addRecord(act);
        }
        
        return validTime;        
    }
    
    /**Removes an activity from the activityLog, dealing with all its consequences
     *
     * @param act
     * @return true if it was removed, false if it didn't exist
     */
    public boolean removeActivity(Activity act){
        boolean actRemoval = this.activityLog.remove(act);
        boolean statRemoval = this.stats.removeActivityStat(act);
        this.restoreRecords(act);
        return (actRemoval && statRemoval);
    }
    
    /**Shows all of a user's statistics
     *
     * @return a string with the statistics
     * @throws StatsNotAvailableException
     */
    public String statsOverview() throws StatsNotAvailableException{
        
        if(this.stats.isEmpty())
            throw(new StatsNotAvailableException() );
        
        String result = this.stats.toString();
        return this.stats.toString();
    }
    
    /**Shows statistics for a given year
     *
     * @param year
     * @return string with the stats
     * @throws StatsNotAvailableException
     */
    public String showAnnualStats(int year) throws StatsNotAvailableException{
        return this.stats.showAnnualStats(year);
    }
    
    /**Shows stats from a given month
     *
     * @param year
     * @param month
     * @return
     * @throws StatsNotAvailableException
     */
    public String showMonthlyStats(int year, int month) throws StatsNotAvailableException{
        return this.stats.showMonthlyStats(year, month);
    }
    
    /**Gets the user's practiced activities
     *
     * @return arraylist of strings with the names of the practiced activities
     */
    public ArrayList<String> getPracticedActivities(){
        ArrayList<String> result = new ArrayList<String>();
        for( Activity a: activityLog){
            if(!result.contains( a.getName() ) )
                    result.add(a.getName());
        }
        return result;
    }
    
    /** Resets records after deleting an activity
     *
     * @param act
     */
    public void restoreRecords(Activity act){
        this.records.removeRecord( act.getName() );
        
        for(Activity a: this.activityLog){
            if( act.getName().equals( a.getName()))
                this.records.addRecord(a);
        }
    }
    
    @Override
    public ActivityInfo clone(){
        return new ActivityInfo(this);
    }
    
    /**Gets the total duration of a given activity in a given year and month
     *
     * @param act
     * @param year
     * @param month
     * @return the total duration
     */
    public long getTotalDuration(String act, int year ,int month){
        return stats.getTotalDuration(act,year,month);
    }
    
    /**Gets the aproximate time to complete 1 Km
     *
     * @param type
     * @return 
     */
    public long getKmTimeAprox(String type){
        return records.getKmTimeAprox(type);
    }
    
    /** Gets the longest distance the user has completed
     *
     * @param type
     * @return the longest distance the user has completed
     */
    public int getMaxRecordDistance(String type){
        return records.getMaxRecordDistance(type);
    }
    
    @Override
    public boolean equals(Object o){
        if( this == o) return true;
        
        if( o == null || this.getClass() != o.getClass() ) return false;
        
        ActivityInfo info = (ActivityInfo) o;
        
        return ( this.stats.equals(info.getStats()) && this.activityLog.equals(info.getActivityLog() ));
    }
    
    @Override
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
