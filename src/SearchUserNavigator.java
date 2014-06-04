import java.util.ArrayList;

/**
 *
 * @author frmendes
 */
public class SearchUserNavigator extends Navigator<User> {

    private FitnessUM app;

    public SearchUserNavigator() {
        super();
        this.app = new FitnessUM();
    }

    public SearchUserNavigator(ArrayList<User> list) {
        super(list);
    }

    public SearchUserNavigator(ArrayList<User> list, FitnessUM app) {
        super(list);
        this.app = app;
    }

    public void print(User u) {
        System.out.println( u.getName() + "\n   " + u.getEmail() );
    }

    public void select(final User u) {
        System.out.println("0. Go Back\n1. View Profile");
        boolean isFriend = app.currentUserHasFriend(u);
        if (isFriend)
            System.out.println("2. Remove Friend");
        else
            System.out.println("2. Add Friend");
        
        int i = Scan.menuOption(0, 2);
        
        Prompt[] options = new Prompt[] {
            new Prompt() { public void exec() { }},
            new Prompt() { public void exec() { System.out.println(u); } },
            new Prompt() { public void exec() { app.addFriend(u); } }
        };
        
        if (i == 2 && isFriend) {
            app.deleteFriend(u);
            this.remove(u);
        }
        else
            options[i].exec();
    }

    public String emptyMessage() {
        return "\nNo results found.\n";
    }
}