
import java.util.ArrayList;

/**
 *
 * @author frmendes
 */
public class FriendListNavigator extends Navigation<BasicUser> {
    public FriendListNavigator() {
        super();
    }
    
    public FriendListNavigator(ArrayList<BasicUser> list) {
        super(list);
    }
    
    public void print(BasicUser u) {
        System.out.println( u.getName() );
    }
    
    public void select(BasicUser u) {
        System.out.println(u);
    }
    
    public String emptyMessage() {
        return "\nUser has no friends\n";
    }
}
