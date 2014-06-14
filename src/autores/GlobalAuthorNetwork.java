package autores;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.NavigableMap;
import java.util.NavigableSet;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

@SuppressWarnings("serial")
public class GlobalAuthorNetwork implements Serializable {
	private TreeMap<Integer, AuthorCatalog> annualNetworks;
	
	public GlobalAuthorNetwork() {
		this.annualNetworks = new TreeMap<Integer, AuthorCatalog>();
	}
	
	/**
	 * Adds a collection of authors to a given year
	 * @param year
	 * @param authors
	 */
	public void addPublication(int year, Collection<String> authors) {
		if( !this.annualNetworks.containsKey(year) ) {
			AuthorCatalog newCatalog = new AuthorCatalog();
			newCatalog.addPublication(authors);
			this.annualNetworks.put(year, newCatalog);
		}
		else {
			this.annualNetworks.get(year).addPublication(authors);
		}
	}
	
	public NavigableMap<Integer, Integer> getYearTable() {
		TreeMap<Integer, Integer> table = new TreeMap<>();
		for(Map.Entry<Integer, AuthorCatalog> entry : this.annualNetworks.entrySet() )
			table.put( entry.getKey(), entry.getValue().getNrPublications() );
		
		return table;
	}
	
	public Tuple<Integer, Integer> getYearInterval() {
		int min = this.annualNetworks.firstKey();
		int max = this.annualNetworks.lastKey();
		return new Tuple<Integer, Integer>(min, max);
	}
	
	/**
	 * Returns the ordered names of the top authors in a given interval
	 * @param interval
	 * @param nrAuthors number of names to be retrieved
	 * @return
	 */
	public NavigableSet<Tuple<String, Integer>> topPublishers(int min, int max, int nrAuthors) {
		TreeMap<String, Integer> authorsTotal = new TreeMap<>();
		for(int i = min; i <= max; i++)
			addYearsTotal(authorsTotal, i);
		
		return GlobalAuthorNetwork.functorAddMax( authorsTotal, nrAuthors, new AuthorPubsTupleComparator() );
	}
	
	/**
	 * Adds the top authors of a given year to the top authors TreeSet
	 * @param authorsTotal
	 * @param year
	 * @param nrAuthors
	 */
	private void addYearsTotal(TreeMap<String, Integer> authorsTotal, int year) {
		AuthorCatalog catalog = this.annualNetworks.get(year);
		if(catalog != null) {
			Map<String, Integer> yearTotal = catalog.authorByPublications();
			GlobalAuthorNetwork.functorMapAdd(authorsTotal, yearTotal);
		}
	}
	
	/**
	 * Returns a NavigableSet of the top coauthor pairs in the given interval
	 * @param years
	 * @param nrAuthors number of pairs to be considered
	 * @return
	 */
	public NavigableSet<Tuple<Tuple<String, String>, Integer>> topPairs(int min, int max, int nrAuthors) {
		TreeMap<Tuple<String, String>, Integer> authorPairs = new TreeMap<>( new AuthorTupleComparator() );

		for(int i = min; i <= max; i++)
				addYearPairs(authorPairs, i);
		
		return GlobalAuthorNetwork.functorAddMax( authorPairs, nrAuthors, new PairPubsTupleComparator() ); // return a clone, please
	}
	
	/**
	 * Adds the top coauthor pairs for the given year to the given TreeSet
	 * @param year
	 * @param authorPairs
	 * @param nrAuthors		number of pairs to be considered
	 */
	private void addYearPairs(TreeMap<Tuple<String, String>, Integer> authorPairs, int year) {
		AuthorCatalog catalog = this.annualNetworks.get(year);
		if(catalog != null) {
			Map<Tuple<String, String>, Integer> yearPairs = catalog.authorPairs();
			
			GlobalAuthorNetwork.functorMapAdd(authorPairs, yearPairs);
		}
	}	

	/**
	 * Returns a NavigableSet with all the authors that were published in every year of the given interval
	 * @param interval
	 * @return
	 * @throws NoAuthorsInIntervalException 
	 */
	public NavigableSet<String> authorsInInterval(Tuple<Integer, Integer> interval) throws NoAuthorsInIntervalException {
		return authorsInInterval( interval.getFirst(), interval.getSecond() );
	}
	
	/**
	 * Returns a NavigableSet with all the authors that were published in every year of the given interval
	 * @param min
	 * @return
	 * @param max
	 * @throws NoAuthorsInIntervalException 
	 */
	public NavigableSet<String> authorsInInterval(int min, int max) throws NoAuthorsInIntervalException {
		NavigableSet<String> authors = getAuthorsInYear(min, max);
		if(authors == null)
			throw new NoAuthorsInIntervalException();
		
		for(int i = min + 1; i <= max; i++) {
			NavigableSet<String> currentYear = getAuthorsInYear(i, max);
			if(currentYear != null) {
				Iterator<String> it = authors.iterator();
				while( it.hasNext() ) {
					if( !currentYear.contains( it.next() ) )
						it.remove();
				}
			}
		}
		
		return authors;
	}
	
	
	/**
	 * Returns a NavigableSet with the authors in min year.
	 * If min year contains no authors it will advance a year until max
	 * If no authors are found, returns null
	 * @param min
	 * @param max
	 * @return
	 */
	private NavigableSet<String> getAuthorsInYear(int min, int max) {
		try {
			return annualNetworks.get(min).getAuthors();
		} catch(NullPointerException e) {
			if(min == max) return null;
			else return getAuthorsInYear(min + 1, max);
		}
	}
	
	
	public int nrAuthorsWithOver(int nrPublications) {
		TreeMap<String, Integer> totals = new TreeMap<>();
		for( AuthorCatalog a : this.annualNetworks.values() )
			GlobalAuthorNetwork.functorMapAdd( totals, a.getAuthorPublications() );
		
		int total = 0;
		for( Integer i : totals.values() )
			if(i > nrPublications) total++;
		
		return total;
	}
	
	/**
	 * Returns a NavigableSet of all the coauthors of a given author
	 * @param name
	 * @return
	 */
	public NavigableSet<String> getCoauthorsOf(String name) {
		TreeSet<String> coauthors = new TreeSet<>();
		for( AuthorCatalog ac : this.annualNetworks.values() )
			if( ac.hasAuthor(name) )
				coauthors.addAll( ac.getCoauthors(name) );
		
		return coauthors;
	}
	
	/**
	 * Returns a set with the authors who only published alone
	 * @return
	 */
	public Set<String> getSoloAuthors() {
		HashMap<String, Boolean> map = new HashMap<>();
		HashSet<String> set = new HashSet<>();
		
		for (AuthorCatalog catalog : this.annualNetworks.values()) {
			for (Map.Entry<String, Boolean> entry : catalog.getSoloAuthors().entrySet()) {
				Boolean b = map.get(entry.getKey());
				
				if (b == null) map.put(entry.getKey(), entry.getValue());
				else map.put(entry.getKey(), b && entry.getValue());
			}
		}
		
		for (Map.Entry<String, Boolean> entry : map.entrySet()) {
			if (entry.getValue()) set.add(entry.getKey());
		}
		return set;
	}
		
	/**
	 * Return the authors who never published alone
	 */
	public Set<String> getNonSoloAuthors() {
		HashMap<String, Boolean> map = new HashMap<>();
		HashSet<String> set = new HashSet<>();
		
		for (AuthorCatalog catalog : this.annualNetworks.values()) {
			for (Map.Entry<String, Boolean> entry : catalog.getNonSoloAuthors().entrySet()) {
				Boolean b = map.get(entry.getKey());
				
				if (b == null) map.put(entry.getKey(), entry.getValue());
				else map.put(entry.getKey(), b && entry.getValue());
			}
		}
		
		for (Map.Entry<String, Boolean> entry : map.entrySet()) {
			if (entry.getValue()) set.add(entry.getKey());
		}
		return set;
	}
	
	/**
	 * Returns the navigable set of common authors to the given authors 
	 * @param head First author to search for
	 * @param tail Rest of the authors to search for
	 * @return
	 */
	public NavigableSet<String> getCommonCoauthors(Collection<String> authors, int min, int max) {
		ArrayList<TreeSet<String>> coauthors = new ArrayList<TreeSet<String>>();
		int i;
		
		for (i = 0; i < authors.size(); i++) coauthors.set(i, new TreeSet<String>());
		
		for (AuthorCatalog catalog : this.annualNetworks.subMap(min, true, max, true).values()) {
			i = 0;
			for (String s : authors) {
				if (catalog.hasAuthor(s)) 
					coauthors.get(i).addAll( catalog.getCoauthors(s) );
			i++;
			}
		}
		
		for (i = 1; i < authors.size(); i++) coauthors.get(0).retainAll(coauthors.get(i));
		
		return coauthors.get(0);
	}
	
	public Tuple<Set<String>, Integer> authorPartnershipInfo(int year, String author) throws NoSuchYearException, NoSuchAuthorException {
		AuthorCatalog an = annualNetworks.get(year); 
		
		if (an == null) throw new NoSuchYearException(year + " does not exist");
		
		return an.authorPartnershipInfo(author);
	}
	
	public NavigableSet<String> getAuthorsBy(char c) {
		TreeSet<String> authors = new TreeSet<>();
		for( AuthorCatalog ac : this.annualNetworks.values() )
			authors.addAll( ac.getAuthorsBy(c) );
		
		
		return authors;
	}
	
	public int totalAuthors() {
		TreeSet<String> authors = new TreeSet<>();
		for( AuthorCatalog ac : this.annualNetworks.values() )
			authors.addAll( ac.getAuthors() );
		
		return authors.size();
	}
	
	/**
	 * Goes through a target Map of &#060T, Integer&#62, adding each value to the existing one in the totals &#060T, Integer&#062 TreeMap.<br>
	 * If the value does not exist, it shall be added.
	 * @param totals
	 * @param target
	 */
	private static <T> void functorMapAdd(Map<T, Integer> totals, Map<T, Integer> target) {
		for( Map.Entry<T, Integer> p : target.entrySet() ) {
			if( totals.containsKey( p.getKey() ) ) {
				int newVal = p.getValue() + totals.get( p.getKey() );
				totals.put(p.getKey(), newVal);
			}
			else
				totals.put( p.getKey(), p.getValue() );
		}
	}
	
	/**
	 * Goes through the totals Map, returning a new NavigableSet ordered by Integer, limited to the given max.
	 * @param totals
	 * @param max
	 * @return
	 */
	private static <T> NavigableSet< Tuple<T, Integer> > functorAddMax(Map<T, Integer> totals, int max, Comparator<Tuple<T, Integer>> c) {
		TreeSet< Tuple<T, Integer> > orderedAuthors = new TreeSet<>(c);
		for( Map.Entry<T, Integer> p : totals.entrySet() ) {
			if(orderedAuthors.size() < max)
				orderedAuthors.add( new Tuple<T, Integer>( p.getKey(), p.getValue() ) );
			else {
				if( p.getValue() > orderedAuthors.first().getSecond() ) {
					orderedAuthors.pollFirst();
					orderedAuthors.add( new Tuple<T, Integer>( p.getKey(), p.getValue() ) );
				}
			}
		}
		return orderedAuthors;
	}
}
