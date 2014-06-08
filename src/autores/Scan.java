package autores;

import java.util.InputMismatchException;
import java.util.Scanner;

/**
 * Class containing a set of Scanners with error handling
 *
 */

public class Scan {
	
	private static Scanner scan;

	/**
	 * Waits for the user to press a single Enter to allow for more displays
	 */
	public static void pressEnterToContinue() {
        System.out.println("\nPress Enter To Continue");
		scan = new Scanner(System.in);

        while ( scan.nextLine().length() > 0 );
    }
	
	/**
	 * Scans the user for a string, handling errors
	 * @param message Message to be displayed
	 * @return
	 */
	public static String scanString(String message) {

		System.out.println(message);
		scan = new Scanner(System.in);
        String s;
		
        try {
        	s = scan.nextLine();
        } catch(InputMismatchException e) {
        	System.out.println("Invalid format");
        	return Scan.scanString(message);
        }
        
        return s;
    }
	
	/**
	 * Scans the user for an int value, handling errors
	 * @param message Message to be displayed
	 * @return
	 */
	public static int scanInt(String message) {
        	System.out.println(message);
        	scan = new Scanner(System.in);
            
	        int val;
	        
	         try {
	            val = scan.nextInt();
	        } catch (Exception e) {
	            System.out.println("Invalid format");
	            val = Scan.scanInt(message);
	        }
	         
	         return val;
	 }
	
	/**
	 * Scans the user for an int value in a given range, handling errors 
	 * @param message Message to be displayed
	 * @param min Minimum value of the range
	 * @param max Maximum value of the range
	 * @return
	 */
	public static int intInRange(String message, int min, int max) {
        int val = Scan.scanInt(message);

        while ( val < min || val > max ) {
            System.out.println("Value is out of bounds\n");
            val = Scan.intInRange(message, min, max);
        }

        return val;
    }
}
