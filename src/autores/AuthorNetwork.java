package autores;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.NavigableSet;
import java.util.Set;

/**
 * Main class of the project, responsible for UI and delegation of user commands to the correct handlers
 *
 */

public class AuthorNetwork {

	private MenuOption[] mainMenu;
	private boolean isActive;
	private Lobby lobby;
	
	
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
	public AuthorNetwork() {
		this.isActive = false;
		this.mainMenu = null;
		this.lobby = new Lobby();
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
			this.lobby.readFromFile(filename);
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
			count = this.lobby.countRepeatedLines(filename);
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
		StringBuilder sb = new StringBuilder();
		sb.append("\nStatistics for ");
		sb.append( this.lobby.getCurrentFile() );
		sb.append("\nTotal number of articles: ");
		sb.append( this.lobby.getTotalPublications() );
		sb.append("\nTotal number of names: ");
		sb.append( this.lobby.getTotalNamesRead() );
		sb.append("\nTotal number of different authors: ");
		sb.append( this.lobby.getTotalAuthors());
		sb.append("\nYear interval: ");
		Tuple<Integer, Integer> interval = this.lobby.getYearInterval();
		sb.append("[" + interval.getFirst() + ", " + interval.getSecond() + "]");
		System.out.println( sb.toString() );
		Scan.pressEnterToContinue();
	}
	
	/**
	 * Prints the statistics for the read file
	 */
	private void getDataStatistics() {
		int nPublications = Scan.scanInt("Enter a number for the author minimum publications");
		
		StringBuilder sb = new StringBuilder();
		sb.append("\nTotal number of solo publications: ");
		sb.append( this.lobby.getSoloPublications() );
		sb.append("\nTotal number of solo authors: ");
		sb.append( this.lobby.getTotalSoloAuthors() );
		sb.append("\nTotal number of non-solo authors: ");
		sb.append( this.lobby.getTotalNonSoloAuthors() );
		sb.append("\nTotal number of authors who published more than " + nPublications + " publications: ");
		sb.append( this.lobby.nrAuthorsWithOver(nPublications) );
		
		System.out.println(sb);
		Scan.pressEnterToContinue();
	}
	
	/**
	 * Prints a table of year-number of publications pair
	 */
	private void getYearTable() {
		for(Map.Entry<Integer, Integer> pair : this.lobby.getYearTable().entrySet() )
			System.out.println(pair.getKey() + ": " + pair.getValue() );
		Scan.pressEnterToContinue();
	}
	
	/**
	 * Prints the list of author names started with a scanned character
	 */
	private void getAuthorsBy() {
		char c = Character.toUpperCase( Scan.scanChar("Enter an initial") );
		
		strNavigation(c + "\n", this.lobby.getAuthorsBy(c));
	}
	
	private void getCommonCoauthors() {
		String[] args;
		do
			args = Scan.scanString("Please enter a maximum of 3 names separated by commas").split(",");
		while(args.length > 3);
		
		String head = args[0].trim();
		ArrayList<String> tail = new ArrayList<String>();
		for( String s : Arrays.asList(args).subList(1, args.length) )
			tail.add( s.trim() );
		
		int min = Scan.scanInt("Please enter the first year");
		int max = Scan.intInRange("Please enter the second year", min + 1, Integer.MAX_VALUE);

		
		NavigableSet<String> res = this.lobby.commonCoauthors(head, tail, min, max);
		
		StringBuilder sb = new StringBuilder();
		sb.append("Common coauthors to " + head);
		for(String s : tail) {
			sb.append(" & ");
			sb.append(s);
		}
		
		sb.append(":\n");
		
		System.out.println(sb);
		
		strNavigation(sb.toString(), res);
	}
	
	
	private void getTopAuthorsInInterval() {
		int min = Scan.scanInt("Please enter the first year");
		int max = Scan.intInRange("Please enter the second year", min, Integer.MAX_VALUE);
		
		int nrAuthors = 0;
		while(nrAuthors < 1)
			nrAuthors = Scan.scanInt("Enter the desired number of authors.");
		
		NavigableSet<Tuple<String, Integer>> authors = this.lobby.topPublishersInInterval(min, max, nrAuthors);
		strIntNavigation("BROL TROL\n", authors.descendingSet());
	}
	
	public void getTopPairsInInterval() {
		int min = Scan.scanInt("Please enter the first year");
		int max = Scan.intInRange("Please enter the second year", min, Integer.MAX_VALUE);
		
		int nrAuthors = 0;
		while(nrAuthors < 1)
			nrAuthors = Scan.scanInt("Enter the desired number of authors.");
		
		NavigableSet<Tuple<Tuple<String, String>, Integer>> authors = this.lobby.topPairs(min, max, nrAuthors);
		strStrIntNavigation("BROL TROL\n", authors);
		
		Scan.pressEnterToContinue();
	}
	
	public void getAuthorsInInterval() {
		int min = Scan.scanInt("Please enter the first year");
		int max = Scan.intInRange("Please enter the second year", min, Integer.MAX_VALUE);
		
		try {
			NavigableSet<String> authors = this.lobby.authorsInInterval(min, max);

			strNavigation("TROL BROL CROL\n", authors);
			
		} catch (NoAuthorsInIntervalException e) {
			System.out.println("No authors available in given interval");
		}
		
		Scan.pressEnterToContinue();
	}
	
	public void getCoauthorInfo() {
		int year = Scan.scanInt("Please enter a year");
		
		Tuple<Integer, Integer> interval = this.lobby.getYearInterval();
		while(year < interval.getFirst() || year > interval.getSecond() )
			year = Scan.scanInt("Invalid year.\nPlease enter a year");
		
		String author = Scan.scanString("Enter an author name.");
		
		Tuple<Set<String>, Integer> info;
		try {
			info = this.lobby.authorPartnershipInfo(year, author);

			strNavigation("Partnership Information:\nTotalPublications: " + info.getSecond() + "\nCo-authors:\n", info.getFirst());
		} catch(NoSuchAuthorException e) {
			System.out.println( e.getMessage() );
		} catch(NoSuchYearException e) {
			System.out.println( e.getMessage() );
		}		
	}
	
	public void getCoauthorsOf() {
		String author = Scan.scanString("Please enter an author name");
		NavigableSet<String> coauthors = this.lobby.getCoauthorsOf(author);
		if( coauthors.size() == 0 )
			System.out.println("Author does not exist");
		else {
			strNavigation("Coauthors of " + author, coauthors);
		}
			
	}
	
	public void save() {
		String filename = Scan.scanString("Enter a filename").trim();
		if(!filename.contains(".obj"))
			filename += ".obj";
		
		try {
			this.lobby.writeToFile(filename);
		} catch (IOException e) {
			System.out.println("Write error");
		}
	}
	
	
	public void load() {
		String filename = Scan.scanString("Enter a filename").trim();
		if(!filename.contains(".obj"))
			filename += ".obj";
		
		try {
			this.lobby = Lobby.readLobbyFromFile(filename);
		} catch (IOException e) {
			System.out.println("Read error");
		} catch(ClassNotFoundException e) {
			System.out.println("Read error, class doesn't exist");
		}
	}
	
	
	/* ##### UI methods ##### */
	
	private static void strStrIntNavigation(String header, Set<Tuple<Tuple<String, String>, Integer>> s) {
		Navigator<Tuple<Tuple<String, String>, Integer>> nav = new Navigator<Tuple< Tuple<String, String>, Integer>>(s);
		PrintFunction<Tuple<Tuple<String, String>, Integer>> pf = new PrintFunction<Tuple<Tuple<String, String>, Integer>>() { public void exec(Tuple<Tuple<String, String>, Integer> arg) { System.out.println(arg.getSecond() + " -\t" + arg.getFirst().getFirst() + " & " + arg.getFirst().getSecond()); } };
		__navigation(nav, pf, header, 20);
	}
	private static void strIntNavigation(String header, Set<Tuple<String, Integer>> s) {
		Navigator<Tuple<String, Integer>> nav = new Navigator<Tuple<String, Integer>>(s);
		PrintFunction<Tuple<String, Integer>> pf = new PrintFunction<Tuple<String, Integer>>() { public void exec(Tuple<String, Integer> arg) { System.out.println(arg.getSecond() + " -\t" + arg.getFirst()); } };
		__navigation(nav, pf, header, 20);
	}
	
	private static void strNavigation(String header, Set<String> s) {
		Navigator<String> nav = new Navigator<>(s);
		PrintFunction<String> pf = new PrintFunction<String>() { public void exec(String arg) { System.out.println(arg); } };
		__navigation(nav, pf, header, 20);
	}
	
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
        		System.out.println("No more items available");
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
		final AuthorNetwork app = this; // Put the app in context to generate its Menu Options
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
		for(String s : AuthorNetwork.mainMenuStrings)
			System.out.println( (i++) + ". " + s);
	}
	
	
	/**
	 * Bootstraps the application, making it ready to go
	 */
	private void bootstrap() {
		this.isActive = true;
		try {
			this.lobby.readFromFile("test/publicx.txt");
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
		AuthorNetwork.printMainMenu();
		
		int option = Scan.intInRange(
				"Please select an option",
				0,
				AuthorNetwork.mainMenuStrings.length - 1
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
		AuthorNetwork.greet();
		new AuthorNetwork().run();
	}

}
