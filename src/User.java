
import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.Set;


/**
 *
 * @author frmendes
 */

public class User extends BasicUser implements BaseModel {
    private int id;
    private FriendList friends;
    private UserInfo info;
    private ActivityInfo activityInfo;

    public User() {
        super();
        this.friends = new FriendList();
        this.info = new UserInfo();
        this.activityInfo = new ActivityInfo();
        this.id = -1;
    }

    public User(String name, String password, String email, UserInfo info) {
        super(name, password, email);
        this.info = info.clone();
        this.friends = new FriendList();
        this.activityInfo = new ActivityInfo();
        this.id = -1;
    }

    public User(String name, String password, String email, FriendList friendlist, UserInfo info, ActivityInfo actInfo) {
        super(name, password, email);
        this.friends = friendlist.clone();
        this.info = info.clone();
        this.activityInfo = actInfo.clone();
        this.id = -1;
    }

    public User(User u) {
        super(u);
        this.friends = u.getFriendList();
        this.info = u.getInfo();
        this.activityInfo = u.getActivityLog();
        this.id = u.getId();
    }

    public int getId() {
        return this.id;
    }

    private FriendList getFriendList() {
        return this.friends.clone();
    }

    public UserInfo getInfo() {
        return info.clone();
    }

    public ActivityInfo getActivityLog() {
        return activityInfo.clone();
    }

    public Milestones getMilestones(String s){
       return activityInfo.getMilestones(s);
   }

    public void setId(int id) {
        this.id = id;
    }

    public void setFriends(UserList friendlist) {
        this.friends.setFriends(friendlist);
    }

    public void setInfo(UserInfo info) {
        this.info = info.clone();
    }

    public void setActivityLog(ActivityInfo actInfo) {
        this.activityInfo = actInfo.clone();
    }


    public void setFriends(FriendList friends) {
        this.friends = friends.clone();
    }

    public void setActivityInfo(ActivityInfo activityInfo) {
        this.activityInfo = activityInfo.clone();
    }


    public void confirmFriendRequest(User u) {
        this.friends.confirmFriendRequest(u);
    }
    public void confirmFriendRequest(int id) {
        this.friends.confirmFriendRequest(id);
    }

    public void sendFriendRequest(User u) {
        this.friends.sendFriendRequest(u);
    }

    public void sendFriendRequest(int id) {
        this.friends.sendFriendRequest(id);
    }

    public void receiveFriendRequest(User u) {
        this.friends.receiveFriendRequest(u);
    }

    public void receiveFriendRequest(int id) {
        this.friends.receiveFriendRequest(id);
    }

    public void acceptFriendRequest(User u) {
        this.friends.acceptFriendRequest(u);
    }

    public void acceptFriendRequest(int id) {
        this.friends.acceptFriendRequest(id);
    }

    public void rejectFriendRequest(User u) {
        this.friends.rejectFriendRequest(u);
    }

    public void rejectFriendRequest(int id) {
        this.friends.rejectFriendRequest(id);
    }

    public void removeSentRequest(User u) {
        this.friends.removeSentRequest(u);
    }

    public void removeSentRequest(int id) {
        this.friends.removeSentRequest(id);
    }

    public void deleteFriend(User u) {
        this.friends.deleteFriend(u);
    }

    public void deleteFriend(int id) {
        this.friends.deleteFriend(id);
    }

    public boolean hasFriend(User u) {
        return hasFriend( u.getId() );
    }

    public boolean hasFriend(int id) {
        return this.friends.hasFriend(id);
    }

    public boolean hasSentRequest(User u) {
        return hasSentRequest( u.getId() );
    }

    public boolean hasSentRequest(int id) {
        return this.friends.hasSentRequest(id);
    }

    public boolean hasReceivedRequest(User u) {
        return hasReceivedRequest( u.getId() );
    }

    public boolean hasReceivedRequest(int id) {
        return this.friends.hasReceivedRequest(id);
    }

    public boolean hasFriendRequest() {
        return this.friends.hasFriendRequest();
    }

    public Set<Integer> getFriends() {
        return this.friends.getFriendList();
    }

    public Set<Integer> getRequests() {
        return this.friends.getRequests();
    }

    public String showStatsOverview() throws StatsNotAvailableException{
        return this.activityInfo.statsOverview();
    }

    public String showAnnualStats(int year) throws StatsNotAvailableException {
        return this.activityInfo.showAnnualStats(year);
    }

    public String showMonthlyStats(int year, int month) throws StatsNotAvailableException {
        return this.activityInfo.showMonthlyStats(year, month);
    }

    public boolean hasPracticed(String type) throws ActivityNotAvailableException {
        return this.activityInfo.getPracticedActivities().contains(type);
    }

    public boolean addActivity(Activity act) {
        boolean res = activityInfo.addActivity(act);
        return res;
    }

    public void removeActivity(Activity act){
        activityInfo.removeActivity(act);
    }

    public ArrayList<Activity> getMostRecentActivities() {
        return activityInfo.getMostRecent();
    }

    public void updateSettings(String name, String email, String pw, UserInfo info) {
        setInfo(info);

        if(name.trim().length() != 0)
            super.setName(name);

        if(email.length() != 0)
            super.setEmail(email);

        super.updatePassword(pw);
    }


    public ArrayList<String> getPracticedActivities(){
        return activityInfo.getPracticedActivities();
    }

    @Override
    public int hashCode() {
        return new Integer( this.getId() ).hashCode();
    }

    @Override
    public User clone() {
        return new User(this);
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        result.append("### User: ###");
        result.append( super.toString() );
        result.append("\nInfo: ");
        result.append(this.info);
        result.append("\nPracticed Activities: ");
        result.append(this.getPracticedActivities().toString() );
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

       return (
               super.equals(o)
               && u.getFriendList().equals(this.friends)
               && u.getInfo().equals(this.info)
               && u.getActivityLog().equals(this.activityInfo)
               && u.getId() == this.id
        );
    }

    public boolean beforeBorn(GregorianCalendar date) {
        if ( date.getTimeInMillis() < this.getInfo().getBirthDate().getTimeInMillis() ) return true;
        return false;
    }

}