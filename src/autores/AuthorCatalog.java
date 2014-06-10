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
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		for(AuthorInfo a : this.authors.values())
			sb.append(a + "\n");
		
		return sb.toString();
	}
	
	/**
	 * Returns a set with the authors who only published alone
	 * @return
	 */
	public Map<String, Boolean> getSoloAuthors() {
		HashMap<String, Boolean> map = new HashMap<String, Boolean>();
		
		for (AuthorInfo author : authors.values()) {
			map.put(author.getName(), author.onlySolo());
		}
		
		return map;
	}
	
	/**
	 * Returns a set with the authors who never published alone
	 * @return
	 */
	public Map<String, Boolean> getNonSoloAuthors() {
		HashMap<String, Boolean> map = new HashMap<String, Boolean>();
		
		for (AuthorInfo author : authors.values()) {
			map.put(author.getName(), author.neverSolo());
		}
		
		return map;
	}
}
