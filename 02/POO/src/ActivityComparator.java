
import java.io.Serializable;
import java.util.Comparator;

/**Compares activities, is used for Trees
 *
 * @author joaorodrigues
 */
public class ActivityComparator implements Comparator<Activity>, Serializable {
    
    @Override
    public int compare(Activity a1, Activity a2){
        int compare = (a1.getDate()).compareTo(a2.getDate());
        if( compare < 0) return 1;
        if( compare > 0) return -1;
        else return 0;
        
   }
}
