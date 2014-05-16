/**
 *
 * @author frmendes
 */

import java.util.ArrayList;
import java.util.Iterator;
import java.util.regex.Pattern;

public class UserController {
    private UserDatabase database;
    private User currentUser;
    
    
    // Name regex - names can't contain more than one space and must not contain any numbers
    private static final String NAMEREGEX = "^[[a-z][A-Z]]*\\s?[[a-z][A-Z]]*$";
    // Email regex
    private static final String EMAILREGEX = "\\A[\\w\\-.]+@[a-z\\-\\d]+(\\.[a-z]+)*\\.[a-z]+\\z";
    
    /**
     * Empty constructor
     */
    
    public UserController() {
        this.database = new UserDatabase();
        this.currentUser = new User();
    }
    
    public UserController(User u, UserDatabase db) {
        this.currentUser = u.clone();
        this.database = db.clone();
    }
    
    public UserController(UserController uc) {
        this.currentUser = uc.getCurrentUser();
        this.database = uc.getDatabase();
    }
    
    public User getCurrentUser() {
        return this.currentUser.clone();
    }
    
    private UserDatabase getDatabase() {
        return this.database.clone();
    }
    
    public boolean validateEmailUniqueness(String email) {
        return this.database.findByEmail(email) == null;
    }
    
    public void registerUser(String name, String email, String password, UserInfo info) {
        this.database.save( new User(name, password, email, info) );
    }
    
    public boolean existsUserWithEmail(String email) {
        return ! this.validateEmailUniqueness(email);
    }
    
    public boolean loginUser(String email, String password) {
        User u = this.database.findByEmail(email);
        boolean match = false;
        
        if ( u.matchPassword(password) ) {
            this.currentUser = u;
            match = true;
        }
        
        return match;
    }
    
    public String currentUserProfile() {
        return this.currentUser.toString();
    }
    
    public ArrayList<BasicUser> getFriendList() {
        ArrayList<BasicUser> friendList = new ArrayList<BasicUser>();
        Iterator<Integer> it = this.currentUser.getFriends().iterator();
        
        while ( it.hasNext() )
            friendList.add( this.database.findById( it.next() ) );
        
        return friendList;
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
        return Pattern.compile(UserController.EMAILREGEX).matcher(email).matches();
    }

    /** Tests if string matches name format
     */
    public static boolean validNameFormat(String name) {
        return Pattern.compile(UserController.NAMEREGEX).matcher(name).matches();
    }
    
}
