package autores;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.NavigableSet;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;


public class AuthorCatalog {
	private HashMap<String, AuthorInfo> authors;
	
	public AuthorCatalog() {
		this.authors = new HashMap<String, AuthorInfo>();
	}
	
	/**
	 * Receives a collection of authors, adding the co-authors to each of them
	 * @param coauthors
	 */
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
	
	/**
	 * Returns the top publishers of a catalog
	 * @param numberOfAuthors number of top authors to be considered
	 * @return
	 */
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
	
	/**
	 * Returns the top pairs of authors, along with their number of publications
	 * @param numberOfPairs number of pairs to be considered
	 * @return
	 */
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
	
	public NavigableSet<String> getAuthors() {
		// Does putAll clone?
		TreeSet<String> authors = new TreeSet<>();
		for( String s : this.authors.keySet() )
			authors.add(s);
		
		return authors;
	}
	
	public Set<String> getCoauthors(String author) {
		AuthorInfo info = this.authors.get(author);
		if(info != null) return info.getCoauthors();
		else return null;
	}
	
	public boolean hasAuthor(String name) {
		return this.authors.keySet().contains(name);
	}
	
	public Map<String, Integer> authorByPublications() {
		TreeMap<String, Integer> authorPubl = new TreeMap<>();
		for( Map.Entry<String, AuthorInfo> info : this.authors.entrySet() )
			authorPubl.put( info.getKey(), info.getValue().getTotalPublications() );
		
		return authorPubl;
	}
	
	public Map<Tuple<String, String>, Integer> authorPairs() {
		TreeMap<Tuple<String, String>, Integer> pairs = new TreeMap<>( new AuthorTupleComparator() );
		for( AuthorInfo a : this.authors.values() )
			pairs.putAll( a.getAuthorPairs() );
		
		return pairs;
	}
}
