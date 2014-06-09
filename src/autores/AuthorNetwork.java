package autores;

import java.io.IOException;
import java.util.Map;
import java.util.NavigableSet;

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
		"Get statistics", "Year Table", "Get Authors By",
		"Get Top Authors In Interval", "Get Top Pairs In Interval", "Get Published Authors In Interval"
	};
	
	
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
			this.lobby.readFromFile(filename);
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
	private void getStatistics() {
		StringBuilder sb = new StringBuilder();
		sb.append("\nStatistics for ");
		sb.append( this.lobby.getCurrentFile() );
		sb.append("\nTotal number of articles: ");
		sb.append( this.lobby.getTotalPublications() );
		sb.append("\nTotal number of authors: ");
		sb.append( this.lobby.getTotalAuthors() );
		sb.append("\nTotal number of solo authors: ");
		sb.append("INFORMATION PENDING"); // replace this
		sb.append("\nTotal number of non-solo authors: ");
		sb.append("INFORMATION PENDING"); // replace this
		sb.append("\nTotal number of different authors: ");
		sb.append("INFORMATION PENDING"); // replace this
		sb.append("\nTotal number of solo publications: ");
		sb.append( this.lobby.getSoloPublications() );
		sb.append("\nYear interval: ");
		Tuple<Integer, Integer> interval = this.lobby.getYearInterval();
		sb.append("[" + interval.getFirst() + ", " + interval.getSecond() + "]");
		System.out.println( sb.toString() );
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
		for(String s : this.lobby.getAuthorsBy(c) )
			System.out.println(s);
		
		Scan.pressEnterToContinue();
	}
	
	
	private void getTopAuthorsInInterval() {
		int min = Scan.scanInt("Please enter the first year");
		int max = Scan.intInRange("Please enter the second year", min, Integer.MAX_VALUE);
		
		int nrAuthors = 0;
		while(nrAuthors < 1)
			nrAuthors = Scan.scanInt("Enter the desired number of authors.");
		
		NavigableSet<Tuple<String, Integer>> authors = this.lobby.topPublishersInInterval(min, max, nrAuthors);
		for(Tuple<String, Integer> a : authors)
			System.out.println("Author: " + a.getFirst() + "\n\t# Publications: " + a.getSecond() +"\n");
		
		Scan.pressEnterToContinue();
	}
	
	public void getTopPairsInInterval() {
		int min = Scan.scanInt("Please enter the first year");
		int max = Scan.intInRange("Please enter the second year", min, Integer.MAX_VALUE);
		
		int nrAuthors = 0;
		while(nrAuthors < 1)
			nrAuthors = Scan.scanInt("Enter the desired number of authors.");
		
		NavigableSet<Tuple<Tuple<String, String>, Integer>> authors = this.lobby.topPairs(min, max, nrAuthors);
		for(Tuple<Tuple<String, String>, Integer> t : authors)
			System.out.println(t.getFirst().getFirst() + " & " + t.getFirst().getSecond() + "\n\t# Publications: " + t.getSecond() +"\n");
		
		Scan.pressEnterToContinue();
	}
	
	public void getAuthorsInInterval() {
		int min = Scan.scanInt("Please enter the first year");
		int max = Scan.intInRange("Please enter the second year", min, Integer.MAX_VALUE);
		
		try {
			NavigableSet<String> authors = this.lobby.authorsInInterval(min, max);
			for(String s : authors)
				System.out.println(s);
		} catch (NoAuthorsInIntervalException e) {
			System.out.println("No authors available in given interval");
		}
		
		Scan.pressEnterToContinue();
	}
	
	/* ##### UI methods ##### */
	
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
				new MenuOption() { public void exec() { app.getStatistics(); } },
				new MenuOption() { public void exec() { app.getYearTable(); } },
				new MenuOption() { public void exec() { app.getAuthorsBy(); } },
				new MenuOption() { public void exec() { app.getTopAuthorsInInterval(); } },
				new MenuOption() { public void exec() { app.getTopPairsInInterval(); } },
				new MenuOption() { public void exec() { app.getAuthorsInInterval(); } }
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
