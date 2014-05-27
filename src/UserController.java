/**
 *
 * @author frmendes
 */

import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.regex.Pattern;

public class UserController {
    private UserDatabase database;
    private User currentUser;
    private boolean adminLogged;


    // Name regex - names can't contain more than one space and must not contain any numbers
    private static final String NAMEREGEX = "^[\\p{L}]*\\s?[\\p{L}]*$";
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
        return this.currentUser.clone();
    }

    private UserDatabase getDatabase() {
        return this.database.clone();
    }

    public boolean isAdminLogin() {
        return this.adminLogged;
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

    public boolean existsUserWithEmail(String email) {
        return !this.validateEmailUniqueness(email);
    }

    public ArrayList<User> nameSearch(String name) {
        return this.database.searchName(name);
    }

    public ArrayList<User> emailSearch(String email) {
        ArrayList<User> al = new ArrayList<User>();
        al.add( this.database.findByEmail(email) );
        return al;
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

    public void addActivity(Activity act){
        currentUser.addActivity(act);
        database.save(currentUser);
    }

    public boolean removeActivity(Activity act){
        boolean result = currentUser.removeActivity(act);
        database.save(currentUser);
        return result;
    }
    
    public ArrayList<Activity> getMostRecentActivities(){
        return currentUser.getMostRecentActivities();
    }

    public String currentUserProfile() {
        return this.currentUser.toString();
    }

    public String showStatsOverview(){
        return this.currentUser.showStatsOverview();
    }
    
    public String showAnnualStats(int year) throws StatsNotAvailable{
        return this.currentUser.showAnnualStats(year);
    }
    
    public String showMonthlyStats(int year, int month) throws StatsNotAvailable{
        return this.currentUser.showMonthlyStats(year, month);
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
        return email.contains("@fitnessum");
    }

}
