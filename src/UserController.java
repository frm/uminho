/**
 *
 * @author frmendes
 */

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;

public class UserController implements Serializable {
    private UserDatabase database;
    private User currentUser;
    private boolean adminLogged;


    // Name regex - names can't contain more than one space and must not contain any numbers
    private static final String NAMEREGEX = "^[\\p{L}]+\\s?[\\p{L}]+$";
    // Email regex
    private static final String EMAILREGEX = "\\A[\\w\\-.]+@[a-z\\-\\d]+(\\.[a-z]+)*\\.[a-z]+\\z";

    /**
     * Empty constructor
     */

    public UserController() {
        this.database = new UserDatabase();
        this.currentUser = new User();
        this.adminLogged = false;
    }

    public UserController(User u, UserDatabase db) {
        this.currentUser = u.clone();
        this.database = db.clone();
        this.adminLogged = false;
    }

    public UserController(UserController uc) {
        this.currentUser = uc.getCurrentUser();
        this.database = uc.getDatabase();
        this.adminLogged = uc.isAdminLogin();
    }

    public User getCurrentUser() {
        if(this.adminLogged)
            return null;

        return this.currentUser.clone();
    }

    private UserDatabase getDatabase() {
        return this.database.clone();
    }

    public boolean isAdminLogin() {
        return this.adminLogged;
    }

    public void logoutAdmin() {
        this.adminLogged = false;
    }

    public boolean validateEmailUniqueness(String email) {
        return this.database.findByEmail(email) == null;
    }

    public boolean validAdminUniqueness(String email) {
        return this.database.findAdmin(email) == null;
    }

    public void registerUser(String name, String email, String password, UserInfo info) {
        this.database.save( new User(name, password, email, info) );
    }

    public void registerAdmin(String name, String password, String email) {
        this.database.addAdmin( new AdminUser(name, password, email) );
    }

    public boolean existsUser(String email) {
        return !this.validateEmailUniqueness(email);
    }

    public boolean existsAdmin(String email) {
        return !this.validAdminUniqueness(email);
    }

    public boolean existsEmail(String email) {
        return existsUser(email) || existsAdmin(email);
    }

    public boolean validUserEmail(String email) {
        return this.validateEmailUniqueness(email) && ! UserController.isAdminEmail(email);
    }

    public boolean validAdminEmail(String email) {
        return this.validAdminUniqueness(email) && UserController.isAdminEmail(email);
    }

    public boolean userParticipatedIn(int id) {
        return this.currentUser.participatedIn(id);
    }
    
    public void joinEvent(int id) {
        this.currentUser.attendEvent(id);
        this.database.save(this.currentUser);
    }
    
    public void leaveEvent(int id) {
        this.currentUser.unattendEvent(id);
        this.database.save(this.currentUser);
    }

    public ArrayList<User> nameSearch(String name) {
        return this.database.searchName(name);
    }

    public ArrayList<User> emailSearch(String email) {
        ArrayList<User> al = new ArrayList<User>();
        al.add( this.database.findByEmail(email) );
        return al;
    }
    
    public ArrayList<User> getUsersWithId(UserList ul) {
        ArrayList<User> list = new ArrayList<User>();
        for( Integer i : ul.getUsers() )
            list.add( this.database.findById(i) );
        
        return list;
    }

    private boolean normalUserLogin(String email, String password) {
        User u = this.database.findByEmail(email);
        boolean match = false;

        if ( u.matchPassword(password) ) {
            this.currentUser = u;
            match = true;
        }

        return match;
    }

    private boolean adminUserLogin(String email, String password) {
        AdminUser au = this.database.findAdmin(email);
        boolean match = false;

        if ( au.matchPassword(password) ) {
            this.currentUser = null;
            this.adminLogged = true;
            match = true;
        }

        return match;
    }

    public boolean loginUser(String email, String password) {
        if ( UserController.isAdminEmail(email) )
            return adminUserLogin(email, password);
        else
            return normalUserLogin(email, password);
    }

    public void sendFriendRequest(User u) {
        if (! this.currentUser.hasSentRequest(u) ) {
            this.currentUser.sendFriendRequest( u.getId() );
            u.receiveFriendRequest( this.currentUser.getId() );
            this.database.save(u);
            this.database.save(this.currentUser);
        }
    }

    public void sendFriendRequest(int id) {
        sendFriendRequest( this.database.findById(id) );
    }

    public void acceptFriendRequest(int id) {
        acceptFriendRequest( this.database.findById(id) );
    }

    public void acceptFriendRequest(User u) {
        if ( this.currentUser.hasReceivedRequest(u) ) {
            this.currentUser.acceptFriendRequest(u);
            u.confirmFriendRequest(this.currentUser);
            this.database.save(u);
            this.database.save(this.currentUser);
        }
    }

    public void rejectFriendRequest(int id) {
        rejectFriendRequest( this.database.findById(id) );
    }

    public void rejectFriendRequest(User u) {
        if ( this.currentUser.hasReceivedRequest(u) ) {
            this.currentUser.rejectFriendRequest(u);
            u.removeSentRequest(this.currentUser);
            this.database.save(u);
            this.database.save(this.currentUser);
        }
    }

    public void deleteFriend(int id) {
        deleteFriend( this.database.findById(id) );
    }

    public void deleteFriend(User u) {
        if ( this.currentUser.hasFriend(u) ) {
            this.currentUser.deleteFriend(u);
            u.deleteFriend(this.currentUser);
            this.database.save(u);
            this.database.save(this.currentUser);
        }
    }

    public boolean hasFriendRequests() {
        return this.currentUser.hasFriendRequest();
    }

    public ArrayList<User> getFriendRequests() {
        ArrayList<User> list = new ArrayList<User>();

        for (int i : this.currentUser.getRequests() )
            list.add( this.database.findById(i) );

        return list;
    }

    public ArrayList<User> getFriendList() {
        ArrayList<User> list = new ArrayList<User>();

        for (int i : this.currentUser.getFriends() )
            list.add( this.database.findById(i) );

        return list;
    }

    public ArrayList<User> getFriendList(int id) {
        User u = this.database.findById(id);
        ArrayList<User> list = new ArrayList<User>();

        for (int i : u.getFriends() )
            list.add( this.database.findById(i) );

        return list;
    }

    private void deleteUserFromFriends(int id) {
        ArrayList<User> friends = this.getFriendList(id);
        for (User u : friends) {
            u.deleteFriend(id);

            this.database.save(u);
        }
    }

    private void deleteUserRequests(int id) {
        for(User u : this.database.all() ) {
            if ( u.hasReceivedRequest(id) ) {
                u.rejectFriendRequest(id);
                this.database.save(u);
            }
        }
    }

    public boolean addActivity(Activity act){
        boolean validActivity = currentUser.addActivity(act);
        if(validActivity) database.save(currentUser);
        return validActivity;
    }

    public Milestones getMilestones(String s){
       return currentUser.getMilestones(s);
    }

    /**
     *
     * @return
     */
    public ArrayList<String> getPracticedActivities() {
        return this.currentUser.getPracticedActivities();
    }

    public void removeActivity(Activity act){
        currentUser.removeActivity(act);
        database.save(currentUser);
    }

    public ArrayList<Activity> getMostRecentActivities(){
        return currentUser.getMostRecentActivities();
    }

    public String currentUserProfile() {
        return this.currentUser.toString();
    }

    public String showStatsOverview() throws StatsNotAvailableException {
        return this.currentUser.showStatsOverview();
    }

    /**
     *
     * @param year
     * @return
     * @throws StatsNotAvailableException
     */
    public String showAnnualStats(int year) throws StatsNotAvailableException {
        return this.currentUser.showAnnualStats(year);
    }

    /**
     *
     * @param year
     * @param month
     * @return
     * @throws StatsNotAvailableException
     */
    public String showMonthlyStats(int year, int month) throws StatsNotAvailableException {
        return this.currentUser.showMonthlyStats(year, month);
    }

    public void writeToFile(String fich) throws IOException{
        ObjectOutputStream oos = new ObjectOutputStream( new FileOutputStream(fich) );
        oos.writeObject(this.database);
        oos.flush(); oos.close();
    }

    public void readFromFile(String fich) throws IOException, ClassNotFoundException{
        ObjectInputStream ois = new ObjectInputStream( new FileInputStream(fich) );
        UserDatabase restored = (UserDatabase) ois.readObject();
        ois.close();
        this.database = restored;
    }

    public void deleteUser(int id) {
        deleteUserFromFriends(id);
        this.database.delete(id);
    }

    public void deleteUser(String email) {
        int id = this.database.findByEmail(email).getId();
        deleteUserFromFriends(id);
        this.database.delete(email);
        deleteUserRequests(id);
    }

    public void deleteUser(User u) {
        deleteUserFromFriends( u.getId() );
        this.database.delete(u);
        deleteUserRequests( u.getId() );
    }

    public void updateUser(String name, String email, String pw, UserInfo info) {
        UserInfo ui = UserInfo.generateValidInfo(this.currentUser.getInfo(), info);
        if (name.length() == 0)
            name = this.currentUser.getName();
        if (email.length() == 0)
            email = this.currentUser.getEmail();

        this.deleteUser( this.currentUser.getId() );
        this.currentUser.updateSettings(name, email, pw, ui);
        this.database.save(currentUser);
    }

    @Override
    public UserController clone() {
        return new UserController(this);
    }

    @Override
    public boolean equals(Object o) {
        if(this == o) return true;
        if( o == null || this.getClass() != o.getClass() ) return false;

        UserController uc = (UserController) o;

       return this.database.equals( uc.getDatabase() ) && this.currentUser == uc.getCurrentUser();
    }

    @Override
    public String toString() {
        StringBuilder str = new StringBuilder();
        str.append("Current user:\n");
        str.append(this.currentUser);
        str.append("User Database");
        str.append(this.database);
        return str.toString();
    }

      /** Tests if string matches email format
     */
    public static boolean validEmailFormat(String email) {
        return UserController.validateRegex(UserController.EMAILREGEX, email);
    }

    /** Tests if string matches name format
     */
    public static boolean validNameFormat(String name) {
        return UserController.validateRegex(UserController.NAMEREGEX, name);
    }

    private static boolean validateRegex(String regex, String str) {
        return Pattern.compile(regex).matcher(str).matches();
    }

    public static boolean isAdminEmail(String email) {
        return email.contains("@fitnessum.com");
    }

    private void addTenRecent(User usr, TreeSet<Tuple<String, Activity>> tree) {
        ArrayList<Activity> tenRecent = usr.getMostRecentActivities();

        boolean full = false;

        for(Activity act: tenRecent) {
            tree.add( new Tuple<String, Activity>( usr.getName(), act.clone() ) );

            if(full) tree.pollLast();
            else if (tree.size() >= 10)
                full = true;
        }

    }

    public Set<Tuple<String, Activity>> getFriendsFeed() throws EmptyFeedException{
        TreeSet<Tuple<String, Activity>> tree = new TreeSet<Tuple<String, Activity>>( new FeedComparator() );
        ArrayList<User> friends = this.getFriendList();

        for(User friend : friends)
            addTenRecent(friend, tree);
        
        if(tree.isEmpty())
            throw new EmptyFeedException();
        
        return tree;
    }



}