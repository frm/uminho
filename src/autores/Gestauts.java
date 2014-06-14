package autores;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.NavigableSet;
import java.util.Set;
import java.util.TreeSet;

/**
 * Main class of the project, responsible for UI and delegation of user commands to the correct handlers
 *
 */

public class Gestauts {

	private MenuOption[] mainMenu;
	private boolean isActive;
	private AuthorNetwork network;
	
	
	private static final String[] mainMenuStrings = {
		"Exit", "Read from file", "Count repeated lines",
		"Get File Statistics", "Get Data Statistics", "Year Table", "Get Authors By",
		"Get Top Authors In Interval", "Get Top Pairs In Interval", "Get Published Authors In Interval",
		"Get Common Coauthors", "Get Coauthor Info", "Get Coauthors Of", "Save", "Load"
	};
	
	private interface PrintFunction<T> {
		public void exec(T arg);
	}
	
	/**
	 * Empty constructor
	 */
	public Gestauts() {
		this.isActive = false;
		this.mainMenu = null;
		this.network = new AuthorNetwork();
		this.generateMainMenu(); 
	}
	
	/* ##### Query methods ##### */
	
	/**
	 * Scans the user for a filename, reading from it
	 */
	private void readFromFile() {
		String filename = Scan.scanString("Enter a filename, please");
		try {
			Crono.start();
			this.network.readFromFile(filename);
			Crono.stop();
			System.out.println(Crono.print());
		} catch(IOException e) {
			System.out.println("File does not exist. Please try again");
			this.readFromFile();
		}
	}
	
	/**
	 * Scans the user for a file name.<br>
	 * Proceeds to count the repeated lines.
	 */
	private void countLines() {
		String filename = Scan.scanString("Enter a filename: ");
		int count;
		
		try {
			Crono.start();
			count = this.network.countRepeatedLines(filename);
			Crono.stop();
			System.out.println(Crono.print());
			System.out.println("Number of repeated lines: " + count);
			Scan.pressEnterToContinue();
		} catch(IOException e) {
			System.out.println("File does not exist. Let's try that again.");
			countLines();
		}
	}
	
	/**
	 * Prints the statistics for the read file
	 */
	private void getFileStatistics() {
		Crono.start();
		StringBuilder sb = new StringBuilder();
		sb.append("\nStatistics for ");
		sb.append( this.network.getCurrentFile() );
		sb.append("\nTotal number of articles: ");
		sb.append( this.network.getTotalPublications() );
		sb.append("\nTotal number of names: ");
		sb.append( this.network.getTotalNamesRead() );
		sb.append("\nTotal number of different authors: ");
		sb.append( this.network.getTotalAuthors());
		sb.append("\nYear interval: ");
		Tuple<Integer, Integer> interval = this.network.getYearInterval();
		sb.append("[" + interval.getFirst() + ", " + interval.getSecond() + "]");
		Crono.stop();
		System.out.println(Crono.print());
		System.out.println( sb.toString() );
		Scan.pressEnterToContinue();
	}
	
	/**
	 * Prints the statistics for the read file
	 */
	private void getDataStatistics() {
		int nPublications = Scan.scanInt("Enter a number for the author minimum publications");
		
		Crono.start();
		
		StringBuilder sb = new StringBuilder();
		sb.append("\nTotal number of solo publications: ");
		sb.append( this.network.getSoloPublications() );
		sb.append("\nTotal number of solo authors: ");
		sb.append( this.network.getTotalSoloAuthors() );
		sb.append("\nTotal number of non-solo authors: ");
		sb.append( this.network.getTotalNonSoloAuthors() );
		sb.append("\nTotal number of authors who published more than " + nPublications + " publications: ");
		sb.append( this.network.nrAuthorsWithOver(nPublications) );
		
		Crono.stop();
		System.out.println(Crono.print());
		
		System.out.println(sb);
		Scan.pressEnterToContinue();
	}
	
	/**
	 * Prints a table of year-number of publications pair
	 */
	private void getYearTable() {
		Crono.start();
		Set<Map.Entry<Integer, Integer>> entrySet = this.network.getYearTable().entrySet();
		Crono.stop();
		System.out.println(Crono.print());
		yearEntryNavigation("Table of Years", entrySet);
	}
	
	/**
	 * Prints the list of author names started with a scanned character
	 */
	private void getAuthorsBy() {
		char c = Character.toUpperCase( Scan.scanChar("Enter an initial") );
		
		Crono.start();
		Set<String> s = this.network.getAuthorsBy(c);
		Crono.stop();
		System.out.println(Crono.print());
		
		strNavigation(c + "\n", s);
	}
	
	/**
	 * Common coauthors view
	 */
	private void getCommonCoauthors() {
		String[] args;
		do
			args = Scan.scanString("Please enter a maximum of 3 names separated by commas").split(",");
		while(args.length > 3);
		
		ArrayList<String> authors = new ArrayList<String>();
		for( String s : Arrays.asList(args))
			authors.add( s.trim() );
		
		int min = Scan.scanInt("Please enter the first year");
		int max = Scan.intInRange("Please enter the second year", min + 1, Integer.MAX_VALUE);

		Crono.start();
		NavigableSet<String> res = this.network.commonCoauthors(authors, min, max);
		Crono.stop();
		System.out.println(Crono.print());
		
		StringBuilder sb = new StringBuilder();
		sb.append("Common coauthors to " + authors.get(0));
		for(String s : authors.subList(1, authors.size())) {
			sb.append(" & ");
			sb.append(s);
		}
		
		sb.append(":\n");
		
		strNavigation(sb.toString(), res);
	}
	
	/**
	 * Top authors in interval view
	 */
	private void getTopAuthorsInInterval() {
		int min = Scan.scanInt("Please enter the first year");
		int max = Scan.intInRange("Please enter the second year", min, Integer.MAX_VALUE);
		
		int nrAuthors = 0;
		while(nrAuthors < 1)
			nrAuthors = Scan.scanInt("Enter the desired number of authors.");
		
		TreeSet<String> authors = new TreeSet<>();
		
		Crono.start();
		Set<Tuple<String, Integer>> s = this.network.topPublishersInInterval(min, max, nrAuthors);
		Crono.stop();
		System.out.println(Crono.print());
		
		for (Tuple<String, Integer> t : s) {
			authors.add(t.getFirst());
		}
		
		strNavigation("Top Authors in [" + min + ", " + max + "]\n", authors);
	}
	
	/**
	 * Top pairs of authors in interval view
	 */
	public void getTopPairsInInterval() {
		int min = Scan.scanInt("Please enter the first year");
		int max = Scan.intInRange("Please enter the second year", min, Integer.MAX_VALUE);
		
		int nrAuthors = 0;
		while(nrAuthors < 1)
			nrAuthors = Scan.scanInt("Enter the desired number of authors.");
		
		Crono.start();
		NavigableSet<Tuple<Tuple<String, String>, Integer>> authors = this.network.topPairs(min, max, nrAuthors);
		Crono.stop();
		System.out.println(Crono.print());
		strStrIntNavigation("BROL TROL\n", authors.descendingSet());
	}
	
	/**
	 * Authors in interval view
	 */
	public void getAuthorsInInterval() {
		int min = Scan.scanInt("Please enter the first year");
		int max = Scan.intInRange("Please enter the second year", min, Integer.MAX_VALUE);
		
		try {
			Crono.start();
			TreeSet<String> t = new TreeSet<String>(this.network.authorsInInterval(min, max));
			Crono.stop();
			System.out.println(Crono.print());
			
			strNavigation("TROL BROL CROL\n", t);
			
		} catch (NoAuthorsInIntervalException e) {
			System.out.println("No authors available in given interval");
		}
		
		Scan.pressEnterToContinue();
	}
	
	/**
	 * Coauthor info view
	 */
	public void getCoauthorInfo() {
		int year = Scan.scanInt("Please enter a year");
		
		Tuple<Integer, Integer> interval = this.network.getYearInterval();
		while(year < interval.getFirst() || year > interval.getSecond() )
			year = Scan.scanInt("Invalid year.\nPlease enter a year");
		
		String author = Scan.scanString("Enter an author name.");
		
		Tuple<Set<String>, Integer> info;
		try {
			Crono.start();
			info = this.network.authorPartnershipInfo(year, author);
			Crono.stop();
			System.out.println(Crono.print());
			
			strNavigation("Partnership Information:\nTotal Publications: " + info.getSecond() + "\nCo-authors:\n", info.getFirst());
		} catch(NoSuchAuthorException e) {
			System.out.println( e.getMessage() );
		} catch(NoSuchYearException e) {
			System.out.println( e.getMessage() );
		}		
	}
	
	/**
	 * Coauthors of an author view
	 */
	public void getCoauthorsOf() {
		String author = Scan.scanString("Please enter an author name");
		
		Crono.start();
		NavigableSet<String> coauthors = this.network.getCoauthorsOf(author);
		Crono.stop();
		System.out.println(Crono.print());
		
		if( coauthors.size() == 0 )
			System.out.println("Author does not exist");
		else {
			strNavigation("Coauthors of " + author, coauthors);
		}
			
	}
	
	/**
	 * Save structure to file view
	 */
	public void save() {
		String filename = Scan.scanString("Enter a filename").trim();
		if(!filename.contains(".obj"))
			filename += ".obj";
		
		try {
			Crono.start();
			this.network.writeToFile(filename);
			Crono.stop();
			System.out.println(Crono.print());
		} catch (IOException e) {
			System.out.println("Write error");
		}
	}
	
	/**
	 * Load structure view
	 */
	public void load() {
		String filename = Scan.scanString("Enter a filename").trim();
		if(!filename.contains(".obj"))
			filename += ".obj";
		
		try {
			Crono.start();
			this.network = AuthorNetwork.readStructureFromFile(filename);
			Crono.stop();
			System.out.println(Crono.print());
		} catch (IOException e) {
			System.out.println("Read error");
		} catch(ClassNotFoundException e) {
			System.out.println("Read error, class doesn't exist");
		}
	}
	
	
	/* ##### UI methods ##### */
	
	/**
	 * Navigation for a set of pairs of years and publications
	 * @param header
	 * @param s
	 */
	private static void yearEntryNavigation(String header, Set<Map.Entry<Integer, Integer>> set) {
		Navigator<Map.Entry<Integer, Integer>> nav = new Navigator<Map.Entry<Integer, Integer>>(set);
		PrintFunction<Map.Entry<Integer, Integer>> pf = new PrintFunction<Map.Entry<Integer, Integer>>() {
			public void exec(Map.Entry<Integer, Integer> entry) {
				System.out.println(entry.getKey() + ": " + entry.getValue());
			}
		};
		__navigation(nav, pf, header, 20);
	}
	
	/**
	 * Navigation for a set of Tuple<Tuple<String, String>, Integer>
	 * @param header
	 * @param set
	 */
	private static void strStrIntNavigation(String header, Set<Tuple<Tuple<String, String>, Integer>> set) {
		Navigator<Tuple<Tuple<String, String>, Integer>> nav = new Navigator<Tuple< Tuple<String, String>, Integer>>(set);
		PrintFunction<Tuple<Tuple<String, String>, Integer>> pf = new PrintFunction<Tuple<Tuple<String, String>, Integer>>() { 
			public void exec(Tuple<Tuple<String, String>, Integer> arg) { 
				System.out.println(arg.getSecond() + " -\t" + arg.getFirst().getFirst() + " & " + arg.getFirst().getSecond()); 
			} 
		};
		__navigation(nav, pf, header, 20);
	}
	
	/*
	private static void strIntNavigation(String header, Set<Tuple<String, Integer>> s) {
		Navigator<Tuple<String, Integer>> nav = new Navigator<Tuple<String, Integer>>(s);
		PrintFunction<Tuple<String, Integer>> pf = new PrintFunction<Tuple<String, Integer>>() { 
			public void exec(Tuple<String, Integer> arg) { 
				System.out.println(arg.getSecond() + " -\t" + arg.getFirst()); 
			} 
		};
		
		__navigation(nav, pf, header, 20);
	}
	*/
	
	/**
	 * Navigation for a set of strings
	 * @param header
	 * @param s
	 */
	private static void strNavigation(String header, Set<String> set) {
		Navigator<String> nav = new Navigator<>(set);
		PrintFunction<String> pf = new PrintFunction<String>() { public void exec(String arg) { System.out.println(arg); } };
		__navigation(nav, pf, header, 20);
	}
	
	/**
	 * Navigation
	 * @param nav
	 * @param pf
	 * @param header
	 * @param blockSize
	 */
	private static <T> void __navigation(Navigator<T> nav, PrintFunction<T> pf, String header, int blockSize) {
        List<T> items = null;
        boolean quit = false;
        
        while (!quit) {
        	try {
        		items = nav.getNext(blockSize);
        		
        		System.out.println("\n" + header);
        		
        		for (T item : items) {
        			pf.exec(item);
        		}
        		
        		System.out.println("\n Showing " + (nav.current() - items.size() + 1)
        				           + " - " + nav.current() + " of " + nav.size() + "\n");
        	}
        	catch (NoMoreItemsException e) {
        		System.out.println("No more items available\n");
        	}
        	finally {
        		int bf = 0;
        		
        		for (;;) {
	        		if (nav.current() > blockSize) {
	        			System.out.print("(B) - Back     ");
	        			bf |= 1;
	        		}
	        		if (nav.itemsLeft() > 0) {
	        			System.out.print("(N) - Next     ");
	        			bf |= 2;
	        		}
	        		System.out.println("(Q) - Quit\n");
	        		
	        		char option = Character.toUpperCase(Scan.scanChar(""));
	        		
	        		if (option == 'B' && (bf & 1) > 0) {
        				try {
        					if (items != null) nav.back(items.size());
        					nav.back(blockSize);
        				}
        				catch (NoMoreItemsException e) {
        					
        				}
	        		}
	        		else if (option == 'N' && (bf & 2) > 0) {
	        			
	        		}
	        		else if (option == 'Q') {
	        			quit = true;
	        		}
	        		else {
	        			System.out.println("Invalid option!");
	        			continue;
	        		}
	        		
	        		break;
        		}
        	}
        }
	}
	
	/**
	 * Print a friendly welcome message
	 */
	private static void greet() {
		System.out.println("Hello and welcome to the Author Network.");
	}
	
	/**
	 * Print a friendly goodbye message, setting the app to inactive 
	 */
	private void shutdown() {
		System.out.println("Bye bye.");
		this.isActive = false;
	}
	
	/**
	 * Generates the main menu option to be selected
	 */
	private void generateMainMenu() {
		final Gestauts app = this; // Put the app in context to generate its Menu Options
		this.mainMenu = new MenuOption[] {
				new MenuOption() { public void exec() { app.shutdown(); } },
				new MenuOption() { public void exec() { app.readFromFile(); } },
				new MenuOption() { public void exec() { app.countLines(); } },
				new MenuOption() { public void exec() { app.getFileStatistics(); } },
				new MenuOption() { public void exec() { app.getDataStatistics(); } },
				new MenuOption() { public void exec() { app.getYearTable(); } },
				new MenuOption() { public void exec() { app.getAuthorsBy(); } },
				new MenuOption() { public void exec() { app.getTopAuthorsInInterval(); } },
				new MenuOption() { public void exec() { app.getTopPairsInInterval(); } },
				new MenuOption() { public void exec() { app.getAuthorsInInterval(); } },
				new MenuOption() { public void exec() { app.getCommonCoauthors(); } },
				new MenuOption() { public void exec() { app.getCoauthorInfo(); } },
				new MenuOption() { public void exec() { app.getCoauthorsOf(); } },
				new MenuOption() { public void exec() { app.save(); } },
				new MenuOption() { public void exec() { app.load(); } }
		};
	}
	
	/**
	 * Prints the main menu options
	 */
	private static void printMainMenu() {
		int i = 0;
		for(String s : Gestauts.mainMenuStrings)
			System.out.println( (i++) + ". " + s);
	}
	
	
	/**
	 * Bootstraps the application, making it ready to go
	 */
	private void bootstrap() {
		this.isActive = true;
		try {
			this.network.readFromFile("test/publicx.txt");
		} catch(IOException e) {
			System.out.println("FATAL ERROR. BASE FILE NOT FOUND");
			this.shutdown();
		}
	}
	
	/**
	 * Runs a command interpreter, printing the options and scanning the user.
	 * It then proceeds to call the selected functionality 
	 */
	public void commandInterpreter() {
		Gestauts.printMainMenu();
		
		int option = Scan.intInRange(
				"Please select an option",
				0,
				Gestauts.mainMenuStrings.length - 1
				);
		
		this.mainMenu[option].exec();
	}
	
	/**
	 * Keeps the app in a cycle while it is active
	 * The user should select the shutdown option to mark it as "inactive"
	 */
	public void run() {
		this.bootstrap();
		while(this.isActive)
			this.commandInterpreter();
	}
	
	/**
	 * Let the party start
	 * @param args
	 */
	public static void main(String[] args) {
		Gestauts.greet();
		new Gestauts().run();
	}

}
