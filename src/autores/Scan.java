package autores;

import java.util.InputMismatchException;
import java.util.Scanner;

/**
 * Class containing a set of Scanners with error handling
 *
 */

public class Scan {
	
	/**
	 * Waits for the user to press a single Enter to allow for more displays
	 */
	public static void pressEnterToContinue() {
        Scanner scan = new Scanner(System.in);
        System.out.println("\nPress Enter To Continue");

        while ( scan.nextLine().length() > 0 );
        
        scan.close();
    }
	
	/**
	 * Scans the user for a string, handling errors
	 * @param message Message to be displayed
	 * @return
	 */
	public static String scanString(String message) {
		Scanner scan = new Scanner(System.in);
        String s;
		System.out.println(message);
		
        try {
        	s = scan.nextLine();
        } catch(InputMismatchException e) {
        	scan.close();
        	System.out.println("Invalid format");
        	return Scan.scanString(message);
        }
        
        scan.close();
        return s;
    }
	
	/**
	 * Scans the user for an int value, handling errors
	 * @param message Message to be displayed
	 * @return
	 */
	public static int scanInt(String message) {
	        Scanner scan = new Scanner(System.in);
	        System.out.println(message);

	        int val;

	         try {
	            val = scan.nextInt();
	        } catch (InputMismatchException e) {
	            System.out.println("Invalid format");
	            scan.close();
	            val = Scan.scanInt(message);
	        }
	         
	         scan.close();
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
