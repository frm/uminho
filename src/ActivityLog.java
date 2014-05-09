
import java.util.Iterator;
import java.util.Objects;
import java.util.Set;
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
public class ActivityLog {
    private TreeSet<Activity> log;
    
    public ActivityLog(){
        this.log = new TreeSet<Activity>( new ActivityComparator() );
    }
    
    public ActivityLog(TreeSet<Activity> log) {
        this.log = (TreeSet<Activity>) log.clone();
    }
    
    public ActivityLog(ActivityLog al) {
        this.log = (TreeSet<Activity>) (al.getLog());
    }
    
    public void setLog(TreeSet<Activity> log) {
        this.log = (TreeSet<Activity>) log.clone();
    }
    
    public Set<Activity> getLog() {
        return (Set<Activity>) this.log.clone();
    }
    
    @Override
    public ActivityLog clone() {
        return new ActivityLog(this);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj == null || this.getClass() != obj.getClass()) {
            return false;
        }
        
        ActivityLog al = (ActivityLog) obj;
        
        return (this.log.equals( al.getLog() ) );
    }
    
    @Override
    public String toString() {
        return "Activity Log: \n"+(this.log);
    }
    
    public void addActivity(Activity act) {
        this.log.add(act);
    }
    
    public boolean removeActivity(Activity act) {
        return this.log.remove(act);
    }
    
    public boolean containsActivity(Activity act) {
        return this.log.contains(act);
    }
    
    /**Get the 10 more recent activities by an user
     *
     * @return Array with the 10 most recent activities
     */
    public Activity[] getMostRecent() {
        int count = 0;
        Activity[] result = new Activity[10];
        Iterator<Activity> it= this.log.iterator();
            
        while (it.hasNext() && count < 10)
            result[count++] = it.next().clone();
        return result;
    }
}
