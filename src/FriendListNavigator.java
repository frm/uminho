
import java.util.ArrayList;

/**
 *
 * @author frmendes
 */
public class FriendListNavigator extends Navigator<User> {

    private FitnessUM app;

    /**
     *
     */
    public FriendListNavigator() {
        super();
        this.app = new FitnessUM();
    }

    /**
     *
     * @param list
     */
    public FriendListNavigator(ArrayList<User> list) {
        super(list);
    }

    /**
     *
     * @param list
     * @param app
     */
    public FriendListNavigator(ArrayList<User> list, FitnessUM app) {
        super(list);
        this.app = app;
    }

    /**
     *
     * @param u
     */
    public void print(User u) {
        System.out.println( u.getName() + "\n   " + u.getEmail() );
    }

    /**
     *
     * @param u
     */
    public void select(final User u) {
        System.out.println("0. Go Back\n1. View Profile\n2. Remove Friend");
        int option = Scan.menuOption(0, 2);
        
		new Prompt[] {
            new Prompt() { public void exec() { }},
            new Prompt() { public void exec() { System.out.println(u); } },
            new Prompt() { public void exec() { app.deleteFriend(u); }}
        }[option].exec();

		if (option == 2) super.remove(u);

    }

    /**
     *
     * @return
     */
    public String emptyMessage() {
        return "\nNo results found.\n";
    }
}