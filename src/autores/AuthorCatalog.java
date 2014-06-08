package autores;

import java.util.Collection;
import java.util.HashMap;
import java.util.NavigableSet;
import java.util.TreeMap;


public class AuthorCatalog {
	private HashMap<String, AuthorInfo> authors;
	
	public AuthorCatalog() {
		this.authors = new HashMap<String, AuthorInfo>();
	}
	
	public void addPublication(int year, Collection<String> coauthors) {
		
		for (String coauthor : coauthors) {
			AuthorInfo info = this.authors.get(coauthor);
			
			if (info == null) {
				info = new AuthorInfo(coauthor);
				this.authors.put(coauthor, info);
			}
			
			info.addPublication(year, coauthors);
		}
	}
	
	public NavigableSet<String> topPublishers(int first, int last, int numberOfAuthors) {
		TreeMap<String, Integer> authorsTotal = new TreeMap<String, Integer>();
		
		return null;
	}
}
