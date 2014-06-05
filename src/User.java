
import java.io.Serializable;
import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Set;


/**
 *
 * @author frmendes
 */

public class User extends BasicUser implements BaseModel{
    private int id;
    private FriendList friends;
    private UserInfo info;
    private ActivityInfo activityInfo;

    /**
     *
     */
    public User() {
        super();
        this.friends = new FriendList();
        this.info = new UserInfo();
        this.activityInfo = new ActivityInfo();
        this.id = -1;
    }

    /**
     *
     * @param name
     * @param password
     * @param email
     * @param info
     */
    public User(String name, String password, String email, UserInfo info) {
        super(name, password, email);
        this.info = info.clone();
        this.friends = new FriendList();
        this.activityInfo = new ActivityInfo();
        this.id = -1;
    }

    /**
     *
     * @param name
     * @param password
     * @param email
     * @param friendlist
     * @param info
     * @param actInfo
     */
    public User(String name, String password, String email, FriendList friendlist, UserInfo info, ActivityInfo actInfo) {
        super(name, password, email);
        this.friends = friendlist.clone();
        this.info = info.clone();
        this.activityInfo = actInfo.clone();
        this.id = -1;
    }

    /**
     *
     * @param u
     */
    public User(User u) {
        super(u);
        this.friends = u.getFriendList();
        this.info = u.getInfo();
        this.activityInfo = u.getActivityLog();
        this.id = u.getId();
    }

    /**
     *
     * @return
     */
    public int getId() {
        return this.id;
    }

    private FriendList getFriendList() {
        return this.friends.clone();
    }

    /**
     *
     * @return
     */
    public UserInfo getInfo() {
        return info.clone();
    }

    /**
     *
     * @return
     */
    public ActivityInfo getActivityLog() {
        return activityInfo.clone();
    }

    /**
     *
     * @param s
     * @return
     */
    public Milestones getMilestones(String s){
       return activityInfo.getMilestones(s);
   }

    /**
     *
     * @param id
     * @return
     */
    public int setId(int id) {
        return this.id = id;
    }

    /**
     *
     * @param friendlist
     */
    public void setFriends(UserList friendlist) {
        this.friends.setFriends(friendlist);
    }

    /**
     *
     * @param info
     */
    public void setInfo(UserInfo info) {
        this.info = info.clone();
    }

    /**
     *
     * @param actInfo
     */
    public void setActivityLog(ActivityInfo actInfo) {
        this.activityInfo = actInfo.clone();
    }

    /**
     *
     * @param friends
     */
    public void setFriends(FriendList friends) {
        this.friends = friends.clone();
    }

    /**
     *
     * @param activityInfo
     */
    public void setActivityInfo(ActivityInfo activityInfo) {
        this.activityInfo = activityInfo.clone();
    }

    /**
     *
     * @param u
     */
    public void confirmFriendRequest(User u) {
        this.friends.confirmFriendRequest(u);
    }

    /**
     *
     * @param id
     */
    public void confirmFriendRequest(int id) {
        this.friends.confirmFriendRequest(id);
    }

    /**
     *
     * @param u
     */
    public void sendFriendRequest(User u) {
        this.friends.sendFriendRequest(u);
    }

    /**
     *
     * @param id
     */
    public void sendFriendRequest(int id) {
        this.friends.sendFriendRequest(id);
    }

    /**
     *
     * @param u
     */
    public void receiveFriendRequest(User u) {
        this.friends.receiveFriendRequest(u);
    }

    /**
     *
     * @param id
     */
    public void receiveFriendRequest(int id) {
        this.friends.receiveFriendRequest(id);
    }

    /**
     *
     * @param u
     */
    public void acceptFriendRequest(User u) {
        this.friends.acceptFriendRequest(u);
    }

    /**
     *
     * @param id
     */
    public void acceptFriendRequest(int id) {
        this.friends.acceptFriendRequest(id);
    }

    /**
     *
     * @param u
     */
    public void rejectFriendRequest(User u) {
        this.friends.rejectFriendRequest(u);
    }

    /**
     *
     * @param id
     */
    public void rejectFriendRequest(int id) {
        this.friends.rejectFriendRequest(id);
    }

    /**
     *
     * @param u
     */
    public void removeSentRequest(User u) {
        this.friends.removeSentRequest(u);
    }

    /**
     *
     * @param id
     */
    public void removeSentRequest(int id) {
        this.friends.removeSentRequest(id);
    }

    /**
     *
     * @param u
     */
    public void deleteFriend(User u) {
        this.friends.deleteFriend(u);
    }

    /**
     *
     * @param id
     */
    public void deleteFriend(int id) {
        this.friends.deleteFriend(id);
    }

    /**
     *
     * @param u
     * @return
     */
    public boolean hasFriend(User u) {
        return hasFriend( u.getId() );
    }

    /**
     *
     * @param id
     * @return
     */
    public boolean hasFriend(int id) {
        return this.friends.hasFriend(id);
    }

    /**
     *
     * @param u
     * @return
     */
    public boolean hasSentRequest(User u) {
        return hasSentRequest( u.getId() );
    }

    /**
     *
     * @param id
     * @return
     */
    public boolean hasSentRequest(int id) {
        return this.friends.hasSentRequest(id);
    }

    /**
     *
     * @param u
     * @return
     */
    public boolean hasReceivedRequest(User u) {
        return hasReceivedRequest( u.getId() );
    }

    /**
     *
     * @param id
     * @return
     */
    public boolean hasReceivedRequest(int id) {
        return this.friends.hasReceivedRequest(id);
    }

    /**
     *
     * @return
     */
    public boolean hasFriendRequest() {
        return this.friends.hasFriendRequest();
    }

    /**
     *
     * @return
     */
    public Set<Integer> getFriends() {
        return this.friends.getFriendList();
    }

    /**
     *
     * @return
     */
    public Set<Integer> getRequests() {
        return this.friends.getRequests();
    }

    /**
     *
     * @return
     * @throws StatsNotAvailable
     */
    public String showStatsOverview() throws StatsNotAvailable{
        return this.activityInfo.statsOverview();
    }

    /**
     *
     * @param year
     * @return
     * @throws StatsNotAvailable
     */
    public String showAnnualStats(int year) throws StatsNotAvailable{
        return this.activityInfo.showAnnualStats(year);
    }

    /**
     *
     * @param year
     * @param month
     * @return
     * @throws StatsNotAvailable
     */
    public String showMonthlyStats(int year, int month) throws StatsNotAvailable{
        return this.activityInfo.showMonthlyStats(year, month);
    }

    /**
     *
     * @param act
     * @return
     */
    public boolean addActivity(Activity act) {
        boolean res = activityInfo.addActivity(act);
        return res;
    }

    /**
     *
     * @param act
     */
    public void removeActivity(Activity act){
        activityInfo.removeActivity(act);
    }

    /**
     *
     * @return
     */
    public ArrayList<Activity> getMostRecentActivities() {
        return activityInfo.getMostRecent();
    }

    /**
     *
     * @param name
     * @param email
     * @param pw
     * @param info
     */
    public void updateSettings(String name, String email, String pw, UserInfo info) {
        setInfo(info);

        if(name.trim().length() != 0)
            super.setName(name);

        if(email.length() != 0)
            super.setEmail(email);

        super.updatePassword(pw);
    }

    /**
     *
     * @return
     */
    public ArrayList<String> getPracticedActivities(){
        return activityInfo.getPracticedActivities();
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
        result.append("\nPracticed Activities: ");
        result.append(this.getPracticedActivities().toString() );
        return result.toString();
    }

    /**
     *
     * @param id
     * @return
     */
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

    /**
     *
     * @param date
     * @return
     */
    public boolean beforeBorn(GregorianCalendar date) {
        if ( date.getTimeInMillis() < this.getInfo().getBirthDate().getTimeInMillis() ) return true;
        return false;
    }

}
