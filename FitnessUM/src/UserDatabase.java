/**
 *
 * @author frmendes
 */

import java.util.Collections;
import java.util.Map;
import java.util.HashMap;

public class UserDatabase {
    
    private HashMap<Integer, User> database;
    private int totalUsers;
    
    
    public UserDatabase() {
        this.totalUsers = 0;
        this.database = new HashMap<Integer, User>();
    }
       
    public UserDatabase(UserDatabase db) {
        this.totalUsers = db.getTotal();
        this.database = (HashMap<Integer, User>)db.all();
    }
    
    public int getTotal() {
        return this.totalUsers;
    }
    
    public Map<Integer, User> all() {
        return (Map<Integer, User>)this.database.clone();
    } 
    
    public User findById(int id) {
        User u = (User)this.database.get(id);
        return u.clone();
    }
    
    public void create(User u) {
        u.setId(this.totalUsers);
        this.database.put(++this.totalUsers, u);
    }
    
    public String toString() {
        return "TOTAL USERS :" + this.totalUsers + " " + this.database;
    }
    
    public boolean equals(Object o) {
        if(this == o) return true;

        if(o == null || this.getClass() != o.getClass() ) return false;

        UserDatabase db = (UserDatabase) o;
        
       return this.totalUsers == db.getTotal() && this.database.equals( db.all() );
    }

    public UserDatabase clone() {
        return new UserDatabase(this);
    }
    
}
