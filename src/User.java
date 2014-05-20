
import java.util.ArrayList;
import java.util.HashMap;


/**
 *
 * @author frmendes
 */

public class User extends BasicUser {
    private UserList friends;
    private UserInfo info;
    private Records records;
    private ActivityLog log;
    private Stats stats;
    
    public User() {
        super();
        this.friends = new UserList(); 
        this.info = new UserInfo();
        this.records = new Records();
        this.log = new ActivityLog();
        this.stats = new Stats();
    }
    
    public User(String name, String password, String email, UserInfo info) {
        super(name, password, email);
        this.info = info.clone();
        this.friends = new UserList();
        this.records = new Records();
        this.log = new ActivityLog();
        this.stats = new Stats();
    }

    public User(String name, String password, String email, UserList friendlist, UserInfo info, Records rec, ActivityLog log, Stats stats) {
        super(name, password, email);
        this.friends = friendlist.clone();
        this.info = info.clone();
        this.records = rec.clone();
        this.log = log.clone();
        this.stats = stats.clone();
    }
    
    public User(User u) {        
        super(u);
        this.friends = u.getFriends();
        this.info = u.getInfo();
        this.records = u.getRecords();
        this.log = u.getActivityLog();
        this.stats = u.getStats();
    }
    
     public UserList getFriends() {
        return friends.clone();
    }
    
    public UserInfo getInfo() {
        return info.clone();
    }
    
    public Records getRecords() {
        return records.clone();
    }
    
    public ActivityLog getActivityLog() {
        return log.clone();
    }

    public Stats getStats() {
        return stats.clone();
    }
    
    public void setFriends(UserList friendlist) {
        this.friends = friendlist.clone();
    }
    
    public void setInfo(UserInfo info) {
        this.info = info.clone();
    }
    
    public void setRecords(Records rec) {
        this.records = rec.clone();
    }
    
    public void setActivityLog(ActivityLog log) {
        this.log = log.clone();
    }
    
    public void setStats(Stats stats) {
        this.stats = stats.clone();
    }
    
    public void addStat(Activity act){
        this.stats.addStat(act);
    }
    
    public String showStats(){
        return this.stats.toString();
    }
    
    public void addFriend(int id) {
        this.friends.addUser(id);
    }
    
    public void addFriend(User u) {
        this.friends.addUser( u.getId() );
    }
    
    public void addActivity(Activity act) {
        log.addActivity(act);
    }
    
    public boolean removeActivity(Activity act) {
        return log.removeActivity(act);
    }
    
    public ArrayList<Activity> getMostRecentActivities() {
        return log.getMostRecent();
    }
    
    public User clone() {
        return new User(this);
    }
    
    public String toString() {
        StringBuilder result = new StringBuilder();
        result.append("### User: ###");
        result.append( super.toString() );
        result.append("\nInfo: ");
        result.append(this.info);
        result.append("\nRecords: ");
        result.append(this.records);
        result.append("\nActivities: ");
        result.append(this.log);

        return result.toString();
    }
    
    public boolean hasFriend(Integer id){
        return this.friends.hasFriend(id);
    }

    
    @Override
    public boolean equals(Object o) {
        if(this == o) return true;

        if(o == null || this.getClass() != o.getClass() ) return false;

        User u = (User) o;
        
       return ( u.getFriends().equals(this.friends) && u.getInfo().equals(this.info) && u.getRecords().equals(this.records) && u.getActivityLog().equals(this.log) && super.equals(o) );
    } 
}
