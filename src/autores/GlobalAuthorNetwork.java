package autores;

import java.util.Collection;
import java.util.NavigableSet;
import java.util.Set;
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
			try {
				addYearsTotal(authorsTotal, i, nrAuthors);
			} catch(NullPointerException e) {}
		
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
	private void addYearsTotal(TreeSet<Tuple<String, Integer>> authorsTotal, int year, int nrAuthors) throws NullPointerException {
		NavigableSet<Tuple<String, Integer>> yearTotal = this.annualNetworks.get(year).topPublishers(nrAuthors);
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
	
	/**
	 * Returns a NavigableSet of the top coauthor pairs in the given interval
	 * @param years
	 * @param nrAuthors number of pairs to be considered
	 * @return
	 */
	public NavigableSet<Tuple<Tuple<String, String>, Integer>> topPairs(Tuple<Integer, Integer> years, int nrAuthors) {
		TreeSet<Tuple<Tuple<String, String>, Integer>> authorPairs = new TreeSet<>( new PairPubsTupleComparator() );

		for(int i = years.getFirst(); i <= years.getSecond(); i++)
			try {
				addYearPairs(i, authorPairs, nrAuthors);
			} catch(NullPointerException e) {}
		
		return authorPairs; // return a clone, please
	}
	
	/**
	 * Adds the top coauthor pairs for the given year to the given TreeSet
	 * @param year
	 * @param authorPairs
	 * @param nrAuthors		number of pairs to be considered
	 */
	private void addYearPairs(int year, TreeSet<Tuple<Tuple<String, String>, Integer>> authorPairs, int nrAuthors) throws NullPointerException {
		NavigableSet<Tuple<Tuple<String, String>, Integer>> yearPairs = this.annualNetworks.get(year).topPairs(nrAuthors);
		for(Tuple<Tuple<String, String>, Integer> pair : yearPairs) {
			if(authorPairs.size() < nrAuthors)
				authorPairs.add(pair);
			else {
				if( pair.getSecond() > authorPairs.first().getSecond() ) {
					authorPairs.pollFirst();
					authorPairs.add(pair);
				}
			}
		}
	}
	
	/**
	 * Returns a NavigableSet with all the authors that were published in every year of the given interval
	 * @param interval
	 * @return
	 */
	public NavigableSet<String> authorsInInterval(Tuple<Integer, Integer> interval) {
		return authorsInInterval( interval.getFirst(), interval.getSecond() );
	}
	
	/**
	 * Returns a NavigableSet with all the authors that were published in every year of the given interval
	 * @param min
	 * @param max
	 * @return
	 */
	public NavigableSet<String> authorsInInterval(int min, int max) {
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
