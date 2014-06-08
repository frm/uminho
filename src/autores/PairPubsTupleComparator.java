package autores;

import java.util.Comparator;

public class PairPubsTupleComparator implements Comparator<Object> {
	
	@SuppressWarnings("unchecked")
	public int compare(Object o1, Object o2) {
		int v;
		Tuple<Tuple<String, String>, Integer> t1 = (Tuple<Tuple<String, String>, Integer>)o1;
		Tuple<Tuple<String, String>, Integer> t2 = (Tuple<Tuple<String, String>, Integer>)o2;
		
		if (t1.getSecond() > t2.getSecond()) return 1;
		else if (t1.getSecond() > t2.getSecond()) return -1;
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
