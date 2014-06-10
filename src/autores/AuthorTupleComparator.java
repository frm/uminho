package autores;

import java.io.Serializable;
import java.util.Comparator;

@SuppressWarnings("serial")
public class AuthorTupleComparator implements Serializable, Comparator<Tuple<String, String>> {
	
	public int compare(Tuple<String, String> t1, Tuple<String, String> t2) {
		int i = t1.getFirst().compareTo( t2.getFirst() );
		if ( i != 0 ) return i;
		else return t1.getSecond().compareTo( t2.getSecond() );
	}
	
}
