package autores;

import java.util.Collection;
import java.util.Map;
import java.util.NavigableSet;
import java.util.TreeMap;
import java.util.TreeSet;

public class GlobalAuthorNetwork {
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
		if( this.annualNetworks.containsKey(year) ) {
			AuthorCatalog newCatalog = new AuthorCatalog();
			newCatalog.addPublication(authors);
			this.annualNetworks.put(year, newCatalog);
		}
		else
			this.annualNetworks.get(year).addPublication(authors);
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
			addYearsTotal(authorsTotal, i, nrAuthors);
		
		return GlobalAuthorNetwork.functorAddMax(authorsTotal, nrAuthors);
	}
	
	/**
	 * Adds the top authors of a given year to the top authors TreeSet
	 * @param authorsTotal
	 * @param year
	 * @param nrAuthors
	 */
	private void addYearsTotal(TreeMap<String, Integer> authorsTotal, int year, int max) {
		AuthorCatalog catalog = this.annualNetworks.get(year);
		if(catalog != null) {
			Map<String, Integer> yearTotal = catalog.authorByPublications();
			GlobalAuthorNetwork.functorMapAdd(authorsTotal, yearTotal, max);
		}
	}
	
	/**
	 * Returns a NavigableSet of the top coauthor pairs in the given interval
	 * @param years
	 * @param nrAuthors number of pairs to be considered
	 * @return
	 */
	public NavigableSet<Tuple<Tuple<String, String>, Integer>> topPairs(Tuple<Integer, Integer> years, int nrAuthors) {
		TreeSet<Tuple<Tuple<String, String>, Integer>> authorPairs = new TreeSet<>( new PairPubsTupleComparator() );

		for(int i = years.getFirst(); i <= years.getSecond(); i++)
				addYearPairs(i, authorPairs, nrAuthors);
		
		return authorPairs; // return a clone, please
	}
	
	/**
	 * Adds the top coauthor pairs for the given year to the given TreeSet
	 * @param year
	 * @param authorPairs
	 * @param nrAuthors		number of pairs to be considered
	 */
	private void addYearPairs(int year, TreeSet<Tuple<Tuple<String, String>, Integer>> authorPairs, int nrAuthors) {
		AuthorCatalog catalog = this.annualNetworks.get(year);
		if(catalog != null) {
			NavigableSet<Tuple<Tuple<String, String>, Integer>> yearPairs = catalog.topPairs(nrAuthors);
			GlobalAuthorNetwork.functorSwap(authorPairs, yearPairs, nrAuthors);
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
	 * @param max
	 * @return
	 * @throws NoAuthorsInIntervalException 
	 */
	public NavigableSet<String> authorsInInterval(int min, int max) throws NoAuthorsInIntervalException {
		NavigableSet<String> authors = getAuthorsInYear(min, max);
		if(authors == null)
			throw new NoAuthorsInIntervalException();
		
		for(int i = min + 1; i <= max; i++) {
			for(String s : getAuthorsInYear(min, max))
				if(!authors.contains(s))
					authors.remove(s);
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
	 * Updates a TreeSet with data from a target.
	 * If max is reached, data is swapped if the first has smaller value than the contendent
	 * @param totals
	 * @param target
	 * @param max
	 */
	private static <T> void functorMapAdd(TreeMap<T, Integer> totals, Map<T, Integer> target, int max) {
		for( Map.Entry<T, Integer> p : target.entrySet() ) {
			if( totals.containsKey(p.getKey() ) ) {
				int newVal = p.getValue() + totals.get( p.getKey() );
				totals.put(p.getKey(), newVal);
			}
			else
				totals.put( p.getKey(), p.getValue() );
		}
	}
	
	private static <T> NavigableSet< Tuple<T, Integer> > functorAddMax(Map<T, Integer> totals, int max) {
		TreeSet< Tuple<T, Integer> > orderedAuthors = new TreeSet<>();
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
