
import java.util.ArrayList;

/**
 *
 * @author frmendes
 */
public class FriendListNavigator extends Navigator<User> {

    private FitnessUM app;

    public FriendListNavigator() {
        super();
        this.app = new FitnessUM();
    }

    public FriendListNavigator(ArrayList<User> list) {
        super(list);
    }

    public FriendListNavigator(ArrayList<User> list, FitnessUM app) {
        super(list);
        this.app = app;
    }

    public void print(User u) {
        System.out.println( u.getName() + "\n   " + u.getEmail() );
    }

    public void select(final User u) {
        System.out.println("0. Go Back\n1. View Profile\n 2. Remove Friend");
        int option = Scan.menuOption(0, 2);
        
		new Prompt[] {
            new Prompt() { public void exec() { }},
            new Prompt() { public void exec() { System.out.println(u); } },
            new Prompt() { public void exec() { app.deleteFriend(u); }}
        }[option].exec();

		if (option == 2) super.remove(u);

    }

    public String emptyMessage() {
        return "\nNo results found.\n";
    }
}