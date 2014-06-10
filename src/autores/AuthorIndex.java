package autores;

import java.util.ArrayList;
import java.util.NavigableSet;
import java.util.Set;
import java.util.TreeSet;

/**
 * Class responsible for keeping the authors name by initial 
 *
 *
 */
public class AuthorIndex {
	ArrayList< TreeSet<String> > index;
	
	public AuthorIndex() {
		this.index = new ArrayList<>(27);
		for(int i = 0; i < 27; i++)
			this.index.add( new TreeSet<String>() );
	}
	
	/**
	 * Adds an author of given name
	 * @param name
	 */
	public void addAuthor(String name) {
		int i = AuthorIndex.getIndex(name);
		this.index.get(i).add(name);
	}
	
	/**
	 * Returns a NavigableSet of authors of a given initial
	 * @param c
	 * @return
	 */
	public NavigableSet<String> getAuthorsBy(char c) {
		TreeSet<String> cpy = new TreeSet<>();
		int i = AuthorIndex.getIndex(c);
		for( String s : this.index.get(i) )
			cpy.add(s);
		
		return cpy;
	}
	
	/**
	 * Returns the total number of authors found in the file read
	 * @return
	 */
	public int totalAuthors() {
		int total = 0;
		
		for (Set<String> s : this.index) {
			total += s.size();
		}
		
		return total;
	}
	
	/**
	 * Returns the index of the array list where the initial of the given name should be
	 * @param name
	 * @return
	 */
	private static int getIndex(String name) {
		return AuthorIndex.getIndex( name.charAt(0) );
	}
	
	/**
	 * Returns the index corresponding to the given initial
	 * @param c
	 * @return
	 */
	private static int getIndex(char c) {
		int i;
		if(c < 'A' || c > 'Z')
			i = 26;
		else
			i = c - 'A';
		
		return i;
	}
	
}
