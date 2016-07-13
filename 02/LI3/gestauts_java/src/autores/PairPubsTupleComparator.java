package autores;

import java.io.Serializable;
import java.util.Comparator;

@SuppressWarnings("serial")
public class PairPubsTupleComparator implements Serializable, Comparator<Tuple<Tuple<String, String>, Integer>> {
	
	public int compare(Tuple<Tuple<String, String>, Integer> t1, Tuple<Tuple<String, String>, Integer> t2) {
		int v;
		if (t1.getSecond() > t2.getSecond()) return 1;
		else if (t1.getSecond() < t2.getSecond()) return -1;
		else {
			if ((v = t1.getFirst().getFirst().compareTo(t2.getFirst().getFirst())) != 0) {
				return v;
			}
			else {
				return t1.getFirst().getSecond().compareTo(t2.getFirst().getSecond());
			}
		}
	}
}
