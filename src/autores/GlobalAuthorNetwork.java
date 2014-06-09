package autores;

import java.util.Collection;
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
	public NavigableSet<String> topPublishers(Tuple<Integer, Integer> interval, int nrAuthors) {
		TreeSet<Tuple<String, Integer>> authorsTotal = new TreeSet<>(new AuthorPubsTupleComparator());
		
		for(int i = interval.getFirst(); i <= interval.getSecond(); i++)
				addYearsTotal(authorsTotal, i, nrAuthors);
		
		TreeSet<String> orderedAuthors = new TreeSet<String>();
		for(Tuple<String, Integer> author : authorsTotal)
			orderedAuthors.add( author.getFirst() );
		
		return orderedAuthors;
	}
	
	/**
	 * Adds the top authors of a given year to the top authors TreeSet
	 * @param authorsTotal
	 * @param year
	 * @param nrAuthors
	 */
	private void addYearsTotal(TreeSet<Tuple<String, Integer>> authorsTotal, int year, int nrAuthors) {
		AuthorCatalog catalog = this.annualNetworks.get(year);
		if(catalog != null) {
			NavigableSet<Tuple<String, Integer>> yearTotal = catalog.topPublishers(nrAuthors);
			for(Tuple<String, Integer> author : yearTotal) {
				if (authorsTotal.size() < nrAuthors)
					authorsTotal.add(author);
				else {
					if( author.getSecond() > authorsTotal.first().getSecond() ) {
						authorsTotal.pollFirst();
						authorsTotal.add(author);
					}
				}
			}
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
	
	private static void functorSwap(TreeSet<Tuple<?, Integer>> totals, NavigableSet<Tuple<?, Integer>> target, int max) {
		for(Tuple<?, Integer> pair : target) {
				if(totals.size() < max)
					totals.add(pair);
				else {
					if( pair.getSecond() > totals.first().getSecond() ) {
						totals.pollFirst();
						totals.add(pair);
				}
			}
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
	public NavigableSet<String> getAuthorsInYear(int min, int max) {
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
	
}
