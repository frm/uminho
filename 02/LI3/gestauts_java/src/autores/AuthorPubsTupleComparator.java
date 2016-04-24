package autores;

import java.io.Serializable;
import java.util.Comparator;

@SuppressWarnings("serial")
public class AuthorPubsTupleComparator implements Serializable, Comparator<Tuple<String, Integer>> {
	
	public int compare(Tuple<String, Integer> t1, Tuple<String, Integer> t2) {
		if (t1.getSecond() > t2.getSecond()) return 1;
		else if (t1.getSecond() < t2.getSecond()) return -1;
		else return t1.getFirst().compareTo(t2.getFirst());
	}
}
