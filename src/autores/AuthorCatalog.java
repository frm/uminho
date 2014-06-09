package autores;

import java.util.Collection;
import java.util.HashMap;
import java.util.NavigableSet;
import java.util.TreeSet;


public class AuthorCatalog {
	private HashMap<String, AuthorInfo> authors;
	
	public AuthorCatalog() {
		this.authors = new HashMap<String, AuthorInfo>();
	}
	
	public void addPublication(Collection<String> coauthors) {
		
		for (String coauthor : coauthors) {
			AuthorInfo info = this.authors.get(coauthor);
			
			if (info == null) {
				info = new AuthorInfo(coauthor);
				this.authors.put(coauthor, info);
			}
			
			info.addPublication(coauthors);
		}
	}
	
	public NavigableSet<Tuple<String, Integer>> topPublishers(int numberOfAuthors) {
		TreeSet<Tuple<String, Integer>> authorsTotal = new TreeSet<>(new AuthorPubsTupleComparator());
		Tuple<String, Integer> t;
		
		for (AuthorInfo info : this.authors.values()) {
			t = new Tuple<String, Integer>(info.getName(), info.getTotalPublications());
			
			if (authorsTotal.size() < numberOfAuthors) {
				authorsTotal.add(t);
			}
			else {
				if (t.getSecond() > authorsTotal.first().getSecond()) {
					authorsTotal.pollFirst();
					authorsTotal.add(t);
				}
			}
		}
		
		return authorsTotal;
	}
	
	public NavigableSet<Tuple<Tuple<String, String>, Integer>> topPairs(int numberOfPairs) {
		TreeSet<Tuple<Tuple<String, String>, Integer>> pairsList = new TreeSet<>(new PairPubsTupleComparator());
		Tuple<Tuple<String, String>, Integer> outerTuple;
		Tuple<String, String> innerTuple;
		
		for (AuthorInfo info : this.authors.values()) {
			for (Tuple<String, Integer> t : info.topCoauthors(numberOfPairs)) {
				if (info.getName().compareTo(t.getFirst()) > 0) {
					innerTuple = new Tuple<String, String>(t.getFirst(), info.getName());
				}
				else {
					innerTuple = new Tuple<String, String>(info.getName(), t.getFirst());
				}
				outerTuple = new Tuple<Tuple<String, String>, Integer>(innerTuple, t.getSecond());
				
				if (pairsList.size() < numberOfPairs) {
					pairsList.add(outerTuple);
				}
				else {
					if (outerTuple.getSecond() > pairsList.first().getSecond()) {
						pairsList.pollFirst();
						pairsList.add(outerTuple);
					}
				}
			}
		}
		
		return pairsList;
	}
}
