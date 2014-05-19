/** UserDatabase class
 * Consists of a double entry table with shared memory
 * Users can be accessed by id or by email
 * @author frmendes
 */

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

public class UserDatabase {

    // Both HashMaps refer to the same user, pointer is shared
    private HashMap<Integer, User> idEntry;              // User indexation by ID
    private HashMap<String, User> emailEntry;         // User indexation by email
    private HashMap<String, User> nameEntry;        // User indexation by name
    private int userCount;                                          // Total number of users that have been saved

    /** Empty constructor
     */
    public UserDatabase() {
        this.idEntry = new HashMap<Integer, User>();
        this.emailEntry = new HashMap<String, User>();
        this.nameEntry = new HashMap<String, User>();
        this.userCount = 0;
    }

    /** Copy constructor
     * @param db UserDatabase to be copied
     */
    public UserDatabase(UserDatabase db) {
        this.idEntry = db.copyIDMap();        
        this.userCount = db.nrUsers();
        
        this.emailEntry = new HashMap<String, User>();
        this.nameEntry = new HashMap<String, User>();
        
        for (User u : this.idEntry.values() ) {
            this.emailEntry.put( u.getEmail(), u);
            this.nameEntry.put( u.getName(), u);
        }
        
    }
    
    /** Parameterized constructor
     * @param ids HashMap of IDs and corresponding Users
     */
    public UserDatabase(HashMap<Integer, User> users)  {
        for (User u : users.values() )
            this.save(u);
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

    /** Returns a user with the corresponding id or null if not found
     * @param id Wanted user id
     * @return Corresponding user
     */
    public User findById(int id) {
        User u = new User();
        try {
            u = this.idEntry.get(id).clone();
        } catch (Exception e) {
            u = null;
        }
        
        return u;
    }
    
    /** Returns a user with the corresponding email or null if not found
     * @param email Wanted user email
     * @return Corresponding user
     */
    public User findByEmail(String email) {
        User u = new User();
        try {
            u = this.emailEntry.get(email).clone();
        } catch (Exception e) {
            u = null;
        }
        
        return u;
    }
    
    public User findByName(String name) {
        User u = new User();
        try {
            u = this.nameEntry.get(name).clone();
        } catch (Exception e) {
            u = null;
        }
        
        return u;
    }
    
    public ArrayList<User> searchName(String name) {
        ArrayList<User> list = new ArrayList<User>();
        
        for (User u : this.nameEntry.values() )
            if ( u.getName().contains(name) )
                list.add( u.clone() );
        
        return list;
    }
    
     public ArrayList<User> searchEmail(String email) {
        ArrayList<User> list = new ArrayList<User>();
        
        for (User u : this.emailEntry.values() )
            if ( u.getEmail().contains(email) )
                list.add( u.clone() );
        
        return list;
    }

    /** Either saves or updates a user
     * If the user id is < 0, it wasn't created and we need to give it an id, otherwise, we override it
     * @param u user to save
     */
    public void save(User u) { 
        User newUser = u.clone();
        
        if ( newUser.getId() < 0 )
            newUser.setId( ++this.userCount );
        
                
        this.idEntry.put( newUser.getId(), newUser );
        this.emailEntry.put( newUser.getEmail(), newUser );
        this.nameEntry.put ( newUser.getName(), newUser );
    }
    
    /** Deletes a user from the network
     * userCount is not updated to avoid ID aliasing
     * @param userId ID of the to delete
     */
    public void delete(int userId) {
        User u = this.findById(userId);
        this.idEntry.remove(userId);
        this.emailEntry.remove( u.getEmail() );
        this.nameEntry.remove( u.getName() );
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

       return this.idEntry.equals( db.copyIDMap() );
    }

    @Override
    public UserDatabase clone() {
        return new UserDatabase(this);
    }
    
    /** Convert a Set of users into a HashMap with valid format
     * @param set Set to be converted
     * @returns converted HashMap
     */
    private static HashMap<Integer, User> userSetToMap(Set<User> set) {
        HashMap<Integer, User> map = new HashMap<Integer, User>();
        for ( User u : set)
            map.put( u.getId(), u );
        
        return map;
    }
    
    /** Copies a HashMap
     * @return copy
     */
    private HashMap<Integer, User> copyIDMap() {
        HashMap<Integer, User> copy = new HashMap<Integer, User>();
        
        for (User u : this.idEntry.values() )
            copy.put( u.getId(), u.clone() );

        return copy;
    }
}
