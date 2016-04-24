/**
 *
 * @author frmendes
 */

import java.util.ArrayList;

/**
 *
 * @author frmendes
 */
public class ManageFriendsNavigator extends SearchUserNavigator {
    
    private FitnessUM app;
    
    /**
     *
     * @param app
     */
    public ManageFriendsNavigator(FitnessUM app) {
        super();
        this.app = app;
    }
    
    /**
     *
     * @param list
     * @param app
     */
    public ManageFriendsNavigator(ArrayList<User> list, FitnessUM app) {
        super(list);
        this.app = app;
    }

    /**
     *
     * @param u
     */
    public void select(User u) {
        this.app.addFriend(u);
    }
}