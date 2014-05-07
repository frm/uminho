/**
 *
 * @author frmendes
 */

import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;


public class UserDatabase {

    private HashMap<Integer, User> database;
    
    /** Constructors
     */
    public UserDatabase() {
        this.database = new HashMap<Integer, User>();
    }

    public UserDatabase(UserDatabase db) {
        this.database = db.copyUserMap();
    }

    public int nrUsers() {
        return this.database.size();
    }

    /** Get all users on the network
     * @return Set of all users
     */
    public Set<User> all() {
        HashSet<User> copy = new HashSet<User>();
        
        for ( User u : this.database.values() )
            copy.add(u);
        
        return (Set<User>)copy;
    }

    /** Returns a user with the corresponding id 
     * @param id Wanted user id
     * @return Corresponding user
     */
    public User findById(int id) {
        User u = (User)this.database.get(id);
        return u.clone();
    }

    /** Either saves or updates a user
     *  If the user id is < 0, it wasn't created and we need to give it an id
     * Otherwise, we override it
     * @param u user to save
     */
    public void save(User u) { 
        if ( u.getId() < 0 )                // if user wasn't put into the db already
            u.setId( this.nrUsers() + 1 );
        
        this.database.put( u.getId(), u);
    }

    @Override
    public String toString() {
        return "TOTAL USERS : " + this.nrUsers() + "\n" + this.database;
    }

    @Override
    public boolean equals(Object o) {
        if(this == o) return true;

        if(o == null || this.getClass() != o.getClass() ) return false;

        UserDatabase db = (UserDatabase) o;

       return this.database.equals( db.copyUserMap() );
    }

    @Override
    public UserDatabase clone() {
        return new UserDatabase(this);
    }
    
    // Convert a Set of users into a HashMap with valid format
    private static HashMap<Integer, User> userSetToMap(Set<User> set) {
        HashMap<Integer, User> map = new HashMap<Integer, User>();
        for ( User u : set)
            map.put( u.getId(), u );
        
        return map;
    }
    
    // Copies a HashMap
    private HashMap<Integer, User> copyUserMap() {
        HashMap<Integer, User> copy = new HashMap<Integer, User>();
        
        for (User u : this.database.values() )
            copy.put( u.getId(), u.clone() );
        
        return copy;
    }
}
