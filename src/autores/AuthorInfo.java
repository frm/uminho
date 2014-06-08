package autores;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

public class AuthorInfo {
	private String name;
	private int totalPublications;
	private TreeMap<Integer, YearInfo> infoByYear;
	
	private class YearInfo {
		private int totalPublications;
		private TreeMap<String, Integer> coauthorsInfo; 
		
		public YearInfo() {
			this.totalPublications = 0;
			this.coauthorsInfo = new TreeMap<String, Integer>();
		}
		
		public int getTotalPublications() {
			return this.totalPublications;
		}
		
		public void incCoauthor(String name) {
			Integer coauthorTotal = this.coauthorsInfo.get(name);
			
			if (coauthorTotal == null) this.coauthorsInfo.put(name, 1);
			else this.coauthorsInfo.put(name, coauthorTotal + 1);
		}
		
		public void inc() {
			this.totalPublications++;
		}
		
		public Map<String, Integer> getCoauthorsInfo() {
			return this.coauthorsInfo;
		}
	}
	
	public AuthorInfo(String name) {
		this.name = name;
		this.totalPublications = 0;
		this.infoByYear = new TreeMap<Integer, YearInfo>();
	}
	
	public int getTotalPublications() {
		return this.totalPublications;
	}
	
	public void addPublication(int year, Collection<String> coauthors) {
		YearInfo yinfo = this.infoByYear.get(year);
		
		if (yinfo == null) {
			yinfo = new YearInfo();
			this.infoByYear.put(year, yinfo);
		}
		
		yinfo.inc();
		
		for (String coauthor : coauthors) {
			if (!coauthor.equals(this.name)) yinfo.incCoauthor(coauthor);
		}
	}
	
	public int publicationsInInterval(int first, int last) {
		Integer total = 0;
		
		for (YearInfo yinfo : this.infoByYear.subMap(first, true, last, true).values()) {
			total += yinfo.getTotalPublications();
		}
		
		return total;
	}
	
	public Set<Tuple<String, Integer>> topCoauthorsInInterval(int first, int last, int numberOfCoauthors) {
		HashMap<String, Integer> coauthors = new HashMap<String, Integer>();
		TreeSet<Tuple<String, Integer>> ret = new TreeSet<Tuple<String, Integer>>(new AuthorPubsTupleComparator());
		Integer total;
		Tuple<String, Integer> t;
		
		for (YearInfo yinfo : this.infoByYear.subMap(first, true, last, true).values()) {
			for (Map.Entry<String, Integer> entry : yinfo.getCoauthorsInfo().entrySet()) {
				total = coauthors.get(entry.getKey());
				if (total == null) {
					coauthors.put(entry.getKey(), entry.getValue());
				}
				else {
					coauthors.put(entry.getKey(), total + entry.getValue());
				}
			}
		}
		
		for (Map.Entry<String, Integer> entry : coauthors.entrySet()) {
			t = new Tuple<String, Integer>(entry.getKey(), entry.getValue());
			
			if (ret.size() < numberOfCoauthors) ret.add(t);
			else if (t.getSecond() > ret.first().getSecond()) {
				ret.pollFirst();
				ret.add(t);
			}
		}
		
		return ret;
	}
	
	public boolean publishedInInterval(int first, int last) {
		return this.infoByYear.subMap(first, true, last, true).size() == (last - first + 1);
	}
}