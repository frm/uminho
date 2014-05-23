
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;


/**
 *
 * @author joaorodrigues
 */
public class ActivityInfo {
    private TreeSet<Activity> log;
    


    /**Get the 10 more recent activities by an user
     *
     * @return Array with the 10 most recent activities
     */
    public ArrayList<Activity> getMostRecent() {
        int count = 0;
        ArrayList<Activity> result = new ArrayList<Activity>();
        Iterator<Activity> it= this.log.iterator();
            
        while (it.hasNext() && count < 10)
            result.add(it.next().clone());
        return result;
    }
}
