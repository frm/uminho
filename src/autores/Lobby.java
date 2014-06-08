package autores;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

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
	
	/**
	 * Empty constructor
	 */
	public Lobby() {
		this.currentFile = "";
	}
	
	/**
	 * Parameterized constructor 
	 * @param filename
	 */
	public Lobby(String filename) {
		this.currentFile = filename;
	}
	
	/**
	 * Sets the read filename
	 * @param filename
	 */
	public void setFilename(String filename) {
		this.currentFile = filename;
	}
	
	/**
	 * Returns the read filename
	 * @return
	 */
	public String getCurrentFile() {
		return this.currentFile;
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
	private Collection<String> getLineArgs(String line) {
		ArrayList<String> args = new ArrayList<>();
		
		for( String s : line.split(",") )
			args.add( s.trim() );
		
		for(String s : args)
			System.out.println(s);
		
		return args;
	}
	
	private void processData(Collection<String> args) {
		
	}
	
	
}
