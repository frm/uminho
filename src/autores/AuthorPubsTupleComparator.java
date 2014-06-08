package autores;

import java.util.Comparator;

public class AuthorPubsTupleComparator implements Comparator<Object> {
	
	@SuppressWarnings("unchecked")
	public int compare(Object o1, Object o2) {
		Tuple<String, Integer> t1 = (Tuple<String, Integer>)o1;
		Tuple<String, Integer> t2 = (Tuple<String, Integer>)o2;
		
		if (t1.getSecond() > t2.getSecond()) return 1;
		else if (t1.getSecond() > t2.getSecond()) return -1;
		else return t1.getFirst().compareTo(t2.getFirst());
	}
}
