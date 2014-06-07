
import java.io.Serializable;
import java.util.Comparator;
/*
 * Class that compares two entries to the user feed
 * The entries shall be ordered by activity date. If the date is the same, then the draw back shall be the name
 */

/**
 *
 * @author frmendes
 */
public class FeedComparator implements Comparator< Tuple<String, Activity> >, Serializable {
    public int compare(Tuple<String, Activity> a1, Tuple<String, Activity> a2) {
        int compare = new ActivityComparator().compare(a1.getValue(), a2.getValue() );
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
