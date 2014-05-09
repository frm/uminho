/** UserDatabase class
 * Consists of a double entry table with shared memory
 * Users can be accessed by id or by email
 * @author frmendes
 */

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

public class UserDatabase {

    // Both HashMaps refer to the same user, pointer is shared
    private HashMap<Integer, User> idEntry;     // User indexation by ID
    private HashMap<String, User> emailEntry;   // User indexation by email
    private int userCount;                      // Total number of users that have been saved
    
    /** Empty constructor
     */
    public UserDatabase() {
        this.idEntry = new HashMap<Integer, User>();
        this.emailEntry = new HashMap<String, User>();
        this.userCount = 0;
    }

    /** Copy constructor
     * @param db UserDatabase to be copied
     */
    public UserDatabase(UserDatabase db) {
        this.idEntry = db.copyIDMap();        
        this.userCount = db.nrUsers();
        
        for (User u : this.idEntry.values() ) {
            this.emailEntry.put( u.getEmail(), u);
            System.out.println("\n\n\nGOT THIS FAR\n\n\n");
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
    }
    
    /** Deletes a user from the network
     * userCount is not updated to avoid ID aliasing
     * @param userId ID of the to delete
     */
    public void delete(int userId) {
        String email = this.findById(userId).getEmail();
        this.idEntry.remove(userId);
        this.emailEntry.remove(email);
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
    
    private static <K,V> HashMap<K, V> copyHashMap(HashMap<K, V> hash) {
        HashMap<K, V> newHash = new HashMap<K, V>();
        Iterator it = hash.entrySet().iterator();
        while ( it.hasNext() ) {
            Map.Entry pair = (Map.Entry)it.next();
            K key = (K)pair.getKey();
            V val = (V)pair.getValue();
            newHash.put( (K)((K)key).clone(), (V)((V)val).clone() );
        }
        
        return newHash;
    }
}
