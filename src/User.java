
import java.util.ArrayList;
import java.util.HashMap;


/**
 *
 * @author frmendes
 */

public class User extends BasicUser {
    private UserList friends;
    private UserInfo info;
    private ActivityLog log;
    private Stats annualStats;
    private Stats monthlyStats;
    
    public User() {
        super();
        this.friends = new UserList(); 
        this.info = new UserInfo();
        this.log = new ActivityLog();
        this.monthlyStats = new Stats();
    }
    
    public User(String name, String password, String email, UserInfo info) {
        super(name, password, email);
        this.info = info.clone();
        this.friends = new UserList();
        this.log = new ActivityLog();
        this.annualStats = new Stats();
        this.monthlyStats = new Stats();
    }

    public User(String name, String password, String email, UserList friendlist, UserInfo info, ActivityLog log, Stats yStats, Stats mStats) {
        super(name, password, email);
        this.friends = friendlist.clone();
        this.info = info.clone();
        this.log = log.clone();
        this.annualStats = yStats.clone();
        this.monthlyStats = mStats.clone();
    }
    
    public User(User u) {        
        super(u);
        this.friends = u.getFriends();
        this.info = u.getInfo();
        this.log = u.getActivityLog();
        this.annualStats = u.getAnnualStats();
        this.monthlyStats = u.getMonthlyStats();
    }
    
     public UserList getFriends() {
        return friends.clone();
    }
    
    public UserInfo getInfo() {
        return info.clone();
    }
    

    
    public ActivityLog getActivityLog() {
        return log.clone();
    }

 
    
    public void setFriends(UserList friendlist) {
        this.friends = friendlist.clone();
    }
    
    public void setInfo(UserInfo info) {
        this.info = info.clone();
    }
    
    
    public void setActivityLog(ActivityLog log) {
        this.log = log.clone();
    }

    public Stats getAnnualStats() {
        return annualStats.clone();
    }

    public Stats getMonthlyStats() {
        return monthlyStats.clone();
    }
    
    
    public void addStat(Activity act){
        this.annualStats.addStat(act);
        this.monthlyStats.addStat(act);
    }
    
    public String showAnnualStats(){
        return this.annualStats.toString();
    }
    
    public String showMonthlyStats(){
        return this.monthlyStats.toString();
    }

    public void setAnnualStats(Stats annualStats) {
        this.annualStats = annualStats.clone();
    }

    public void setMonthlyStats(Stats monthlyStats) {
        this.monthlyStats = monthlyStats.clone();
    }
    
    public String showAnnualStats(String name){
        return this.annualStats.getActivityStat(name).toString();
    }
    
    public String showMonthlyStats(String name){
        return this.monthlyStats.getActivityStat(name).toString();
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
        
       return ( u.getFriends().equals(this.friends) && u.getInfo().equals(this.info)  && u.getActivityLog().equals(this.log) && super.equals(o) );
    } 
}
