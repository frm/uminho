package autores;

/**
* Contains all the authors and the corresponding information in a year
*/
import java.io.Serializable;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.NavigableSet;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;


@SuppressWarnings("serial")
public class AuthorCatalog implements Serializable {
	private HashMap<String, AuthorInfo> authors;
	private int nrPublications;

	public AuthorCatalog() {
		this.authors = new HashMap<String, AuthorInfo>();
		this.nrPublications = 0;
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

		this.nrPublications++;
	}

	/**
	 * Returns the number of publications of a year
	 * @return
	 */
	public int getNrPublications() {
		return this.nrPublications;
	}

	public NavigableSet<String> getAuthorsBy(char c) {
		TreeSet<String> authors = new TreeSet<>();
		for( AuthorInfo ai : this.authors.values() )
			if( ai.getName().charAt(0) == c )
				authors.add( ai.getName() );

		return authors;
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
	 * Returns the authors that published in the given year
	 * @return
	 */
	public NavigableSet<String> getAuthors() {
		TreeSet<String> authors = new TreeSet<>();
		for( String s : this.authors.keySet() )
			authors.add(s);

		return authors;
	}

	/**
	 * Returns the coauthors of a given author
	 * @param author
	 * @return
	 */
	public Set<String> getCoauthors(String author) {
		AuthorInfo info = this.authors.get(author);
		if(info != null) return info.getCoauthors();
		else return null;
	}

	/**
	 * Checks if an authors with the given name has published in current year
	 * @param name
	 * @return
	 */
	public boolean hasAuthor(String name) {
		return this.authors.keySet().contains(name);
	}

	/**
	 * Returns a map of author name and the number of publications in current year
	 * @return
	 */
	public Map<String, Integer> authorByPublications() {
		TreeMap<String, Integer> authorPubl = new TreeMap<>();
		for( Map.Entry<String, AuthorInfo> info : this.authors.entrySet() )
			authorPubl.put( info.getKey(), info.getValue().getTotalPublications() );

		return authorPubl;
	}

	/**
	 * Returns each pair of authors in current year
	 * @return
	 */
	public Map<Tuple<String, String>, Integer> authorPairs() {
		TreeMap<Tuple<String, String>, Integer> pairs = new TreeMap<>( new AuthorTupleComparator() );
		for( AuthorInfo a : this.authors.values() )
			pairs.putAll( a.getAuthorPairs() );

		return pairs;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		for(AuthorInfo a : this.authors.values())
			sb.append(a + "\n");

		return sb.toString();
	}

	/**
	 * Returns a map of the author name and the total number of publications in the current year
	 * @return
	 */
	public Map<String, Integer> getAuthorPublications() {
		TreeMap<String, Integer> pairs = new TreeMap<>();
		for( AuthorInfo a : this.authors.values() )
			pairs.put( a.getName(), a.getTotalPublications() );

		return pairs;
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

	/**
	 * Returns a tuple containing a set of coauthors and total number of coauthor publications for the author with the given name
	 * Shall throw a NoSuchAuthorException when the author doesn't exist
	 * @param author
	 * @return
	 * @throws NoSuchAuthorException
	 */
	public Tuple<Set<String>, Integer> authorPartnershipInfo(String author) throws NoSuchAuthorException {
		AuthorInfo aut = authors.get(author);

		if (aut == null) {
			throw new NoSuchAuthorException(author + " not found.");
		}
		else {
			return aut.partnershipInfo();
		}
	}
}
