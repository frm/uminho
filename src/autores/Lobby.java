package autores;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * Class responsible for:<br>
 * <ul>
 * 		<li>Reading from a file;</li>
 * 		<li>Populating the database accordingly;</li>
 * 		<li>Communicating with the structures</li>
 * </ul>
 *
 */




public class Lobby {
	private String currentFile;
	private int lineNumber;
	private Statistics stats;
	
	/**
	 * Empty constructor
	 */
	public Lobby() {
		this.currentFile = "";
		this.lineNumber = 0;
		this.stats = new Statistics();
	}
	
	/**
	 * Sets the read filename
	 * @param filename
	 */
	public void setFilename(String filename) {
		this.currentFile = filename;
	}
	
	/**
	 * Returns the number of articles read
	 * @return
	 */
	public int getNumberArticles() {
		return this.lineNumber;
	}
	
	/**
	 * Returns the read filename
	 * @return
	 */
	public String getCurrentFile() {
		return this.currentFile;
	}
	
	public int getSoloPublications() {
		return stats.getSoloArticles();
	}
	
	public int getTotalAuthors() {
		return stats.getTotalNames();
	}
	
	public int getTotalPublications() {
		return stats.getTotalArticles();
	}
	
	public Tuple<Integer, Integer> getYearInterval() {
		return stats.getYearInterval();
	}
	
	/**
	 * Reads from a file, populating the database
	 * @param filename name of the file to be read
	 */
	public void readFromFile(String filename) {
		setFilename(filename);
		
		try ( BufferedReader br = new BufferedReader( new FileReader(filename) ) ) {
			
			String line = br.readLine();
			
			while(line != null) {
				processData( getLineArgs(line) );
				line = br.readLine();
			}
			
		} catch (IOException e) {
			e.printStackTrace();
		}
		
	}
	
	/**
	 * Receives a line, splitting it into valid information to be processed
	 * The information shall be returned as a Collection
	 * @param line
	 * @return
	 */
	private List<String> getLineArgs(String line) {
		ArrayList<String> args = new ArrayList<>();
		
		for( String s : line.split(",") )
			args.add( s.trim() );
		
		return args;
	}
	
	/**
	 * Processes the data, inserting to databases
	 * @param args
	 */
	private void processData(List<String> args) {
		// do something more for the catalog
		this.stats.process(args);
		this.lineNumber++;
	}
	
	/**
	 * Counts the number of repeated lines for a given file.
	 * @param filename
	 * @return
	 */
	public int countRepeatedLines(String filename) {
		TreeSet<String> lineTree = new TreeSet<>();
		int repeatedLines = 0;
		
		try ( BufferedReader br = new BufferedReader( new FileReader(filename) ) ) {
			
			String line = br.readLine();
			
			while(line != null) {
				if( lineTree.contains(line) ) repeatedLines++;
				else lineTree.add(line);
				
				line = br.readLine();
			}
			
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		return repeatedLines;
	}

	private class Statistics {
		private int totalArticles;
		private int totalNames;
		private int soloArticles;
		private TreeMap<Integer, Integer> yearTable;
		
		public Statistics() {
			this.totalArticles = 0;
			this.totalNames = 0;
			this.soloArticles = 0;
			this.yearTable = new TreeMap<>();
		}
		
		public int getTotalArticles() {
			return this.totalArticles;
		}
		
		public int getTotalNames() {
			return this.totalNames;
		}
		
		public int getSoloArticles() {
			return this.soloArticles;
		}
		
		public Tuple<Integer, Integer> getYearInterval() {
			return new Tuple<>( this.yearTable.firstKey(), this.yearTable.lastKey() );			
		}
		
		public void process(List<String> publication) {
			updateTotals(publication);
			int year = Integer.parseInt( publication.get(publication.size() - 1) );
			updateTable(year);
		}
		
		private void updateTable(int year) {
			
			if( !this.yearTable.containsKey(year) ) {
				int oldTotal = this.yearTable.get(year);
				this.yearTable.put(year, oldTotal + 1);
			}
			
			else
				this.yearTable.put(year, 0);
		}
		
		private void updateTotals(List<String> publication) {
			this.totalArticles++;
			this.totalNames += (publication.size() - 1);
			if(publication.size() == 2)	// if the publication only has one author
				this.soloArticles++;
		}
	}	
	
}
