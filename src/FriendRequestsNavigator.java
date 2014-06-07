import java.util.ArrayList;

/**
 *
 * @author frmendes
 */
public class FriendRequestsNavigator extends Navigator<User> {

    private FitnessUM app;

    public FriendRequestsNavigator() {
        super();
        this.app = new FitnessUM();
    }

    public FriendRequestsNavigator(ArrayList<User> list) {
        super(list);
    }

    public FriendRequestsNavigator(ArrayList<User> list, FitnessUM app) {
        super(list);
        this.app = app;
    }

    public void print(User u) {
        System.out.println( u.getName() + "\n   " + u.getEmail() );
    }

    public void select(final User u) {
        System.out.println("0. Go Back\n1. View Profile\n2. Accept\n3. Reject");
        int option = Scan.menuOption(0, 3);
        new Prompt[] {
            new Prompt() { public void exec() { }},
            new Prompt() { public void exec() { System.out.println(u); } },
            new Prompt() { public void exec() { app.acceptFriend(u); }},
            new Prompt() { public void exec() { app.rejectFriend(u); }}
        }[option].exec();
        
        if(option == 2 || option == 3)
            remove(u);
    }

    public String emptyMessage() {
        return "\nNo results found.\n";
    }
}