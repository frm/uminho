
import java.util.ArrayList;


/**
 *
 * @author tiago
 */
public class RecordsNavigator extends Navigator<String>{
    private UserController uc;

    /**
     *
     */
    public RecordsNavigator() {
        super();
        this.uc = new UserController();
    }

    /**
     *
     * @param list
     * @param userc
     */
    public RecordsNavigator(ArrayList<String> list, UserController userc ){
        super(list);
        this.uc = userc.clone();
    }

    /**
     *
     * @param s
     */
    public void print(String s) {
        System.out.println( s );
    }

    /**
     *
     * @param s
     */
    public void select(String s){
        System.out.println( this.uc.getMilestones(s) );
    }

    /**
     *
     * @return
     */
    public String emptyMessage() {
        return "\nUser has no activities\n";
    }
}
