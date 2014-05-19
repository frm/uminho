/**
 *
 * @author frmendes
 */

import java.util.ArrayList;

/**
 *
 * @author frmendes
 */
public class SearchUserNavigator extends Navigator<User> {
    
    public SearchUserNavigator() {
        super();
    }
    
    public SearchUserNavigator(ArrayList<User> list) {
        super(list);
    }
    
    public void print(User u) {
        System.out.println( u.getName() + "\n   " + u.getEmail() );
    }
    
    public void select(User u) {
        System.out.println(u);
    }
    
    public String emptyMessage() {
        return "\nNo results found.\n";
    }  
}