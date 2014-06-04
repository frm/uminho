
import java.io.Serializable;
import java.util.Comparator;
import java.util.Map;

/*
 * Class that compares two entries to the user feed
 * The entries shall be ordered by activity date. If the date is the same, then the draw back shall be the name
 */

/**
 *
 * @author frmendes
 */
public class FeedComparator implements Comparator <Map.Entry<String, Activity> >, Serializable {
    public int compare(Map.Entry<String, Activity> a1, Map.Entry<String, Activity> a2) {
        int compare = a1.getValue().getDate().compareTo( a2.getValue().getDate() );
        if (compare < 0) return 1;
        if (compare > 0) return -1;
        else {
            int compareName = a1.getKey().compareTo(  a2.getKey() );
            if(compareName < 0) return 1;
            if(compareName > 0) return -1;
            else return 0;
        }
    }
}
