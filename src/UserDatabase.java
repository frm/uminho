/** UserDatabase class
 * Consists of a double entry table with shared memory
 * Users can be accessed by id or by email
 * @author frmendes
 */

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

public class UserDatabase implements Serializable{

    // Both HashMaps refer to the same user, pointer is shared
    private HashMap<Integer, User> idEntry;              // User indexation by ID
    private HashMap<String, Integer> emailEntry;         // User indexation by email
    private HashMap<String, AdminUser> adminEntry;       // Admin Indexation by Email
    private int userCount;                                          // Total number of users that have been saved

    /** Empty constructor
     */
    public UserDatabase() {
        this.idEntry = new HashMap<Integer, User>();
        this.emailEntry = new HashMap<String, Integer>();
        this.adminEntry = new HashMap<String, AdminUser>();
        this.userCount = 0;
    }

    /** Copy constructor
     * @param db UserDatabase to be copied
     */
    public UserDatabase(UserDatabase db) {
        this.idEntry = new HashMap<Integer, User>();
        this.emailEntry = new HashMap<String, Integer>();

        this.userCount = db.nrUsers();

        this.adminEntry = UserDatabase.copyAdmins( db.getAdmins() );
        UserDatabase.copyUsers(this, db);

    }

    /** Parameterized constructor
     * @param ids HashMap of IDs and corresponding Users
     */
    public UserDatabase(HashMap<Integer, User> users, HashMap<String, AdminUser> admins)  {
        this.adminEntry = UserDatabase.copyAdmins(admins);
        for (User u : users.values() )
            this.save(u);
    }

    private HashMap<Integer, User> getIdEntry() {
        HashMap<Integer, User> cpy = new HashMap<Integer, User>();
       for (User u : this.idEntry.values() )
            cpy.put( u.getId(), u.clone() );
       
        return cpy;
    }

    private HashMap<String, Integer> getEmailEntry() {
        HashMap<String, Integer> cpy = new HashMap<String, Integer>();
        cpy.putAll(this.emailEntry);
        return cpy;
    }

    /** Getter for number of users on the network
     * @return Total number of users
     */
    public int nrUsers() {
        return this.userCount;
    }

    /** Get all users on the network
     * @return Set of all users
     */
    public Set<User> all() {
        HashSet<User> copy = new HashSet<User>();

        for ( User u : this.idEntry.values() )
            copy.add( u.clone() );

        return (Set<User>)copy;
    }

    private HashMap<String, AdminUser> getAdmins() {
        return UserDatabase.copyAdmins(this.adminEntry);
    }

    /** Returns a user with the corresponding id or null if not found
     * @param id Wanted user id
     * @return Corresponding user
     */
    public User findById(int id) {
        User u;
        try {
            u = this.idEntry.get(id).clone();
        } catch (NullPointerException e) {
            u = null;
        }
        return u;
    }

    /** Returns a user with the corresponding email or null if not found
     * @param email Wanted user email
     * @return Corresponding user
     */
    public User findByEmail(String email) {
        int id;
        try {
            id = this.emailEntry.get(email);
        } catch (NullPointerException e) {
            return null;
        }
        
        return findById(id);
    }

    public AdminUser findAdmin(String email) {
        AdminUser a;
        try {
            a = this.adminEntry.get(email).clone();
        } catch (NullPointerException e) {
            a = null;
        }
        
        return a;
    }

    public ArrayList<User> searchName(String name) {
        ArrayList<User> list = new ArrayList<User>();

        for (User u : this.idEntry.values() )
            if ( u.getName().contains(name) )
                list.add( u.clone() );

        return list;
    }

     public void addAdmin(AdminUser au) {
         this.adminEntry.put( au.getEmail(), new AdminUser(au) );
     }

     public void addAdmin(String name, String password, String email) {
         addAdmin( new AdminUser(name, password, email) );
     }

    /** Either saves or updates a user
     * If the user id is < 0, it wasn't created and we need to give it an id, otherwise, we override it
     * @param u user to save
     */
    public void save(User u) {
        User newUser = u.clone();

        if ( u.getId() < 0 )
            newUser.setId( ++this.userCount );


        this.idEntry.put( newUser.getId(), newUser );
        this.emailEntry.put( newUser.getEmail(), newUser.getId() );
    }

    /** Deletes a user from the network
     * userCount is not updated to avoid ID aliasing
     * @param userId ID of the to delete
     */
    public void delete(int userId) {
        String email = findById(userId).getEmail();
        this.idEntry.remove(userId);
        this.emailEntry.remove(email);
    }
    
    public void delete(String email) {
       int id = this.emailEntry.get(email);
       this.idEntry.remove(id);
       this.emailEntry.remove(email);
    }

    public void delete(User u) {
        this.idEntry.remove( u.getId() );
        this.emailEntry.remove( u.getEmail() );
    }

    @Override
    public String toString() {
        return "TOTAL USERS : " + this.nrUsers() + "\n" + this.idEntry + "\n" + this.emailEntry;
    }

    @Override
    public boolean equals(Object o) {
        if(this == o) return true;

        if( o == null || this.getClass() != o.getClass() ) return false;

        UserDatabase db = (UserDatabase) o;

       return this.idEntry.equals( db.getIdEntry() ) && this.emailEntry.equals( db.getEmailEntry() ) && this.adminEntry.equals( db.getAdmins() );
    }

    @Override
    public UserDatabase clone() {
        return new UserDatabase(this);
    }

    private static HashMap<String, AdminUser> copyAdmins(HashMap<String, AdminUser> admins) {
        HashMap<String, AdminUser> adm = new HashMap<String, AdminUser>();

        for ( AdminUser au : admins.values() )
            adm.put( au.getEmail(), au.clone() );

        return adm;
    }

    private static void copyUsers(UserDatabase db1, UserDatabase db2) {
        for ( User u : db2.all() ) {
            int id = u.getId();
            db1.idEntry.put(id, u);
            db1.emailEntry.put( u.getEmail(), id );
        }
    }

}
