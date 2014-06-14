package autores;

import java.util.InputMismatchException;
import java.util.Scanner;
import static java.lang.System.out;
import static java.lang.System.in;

/**
 * Classe que abstrai a utilização da classe Scanner, escondendo todos os
 * problemas relacionados com excepções, e que oferece métodos simples e
 * robustos para a leitura de valores de tipos simples.
 *
 * -----  Utilização: Exemplos
 *
 * int i = Input.lerInt();
 * String linha = Input.lerString();
 * double raio = Input.lerDouble();
 * ---------------------------------------
 *
 * @author F. Mário Martins
 * @version 1.0 (6/2006)
 */


public class Input {
	
	private static Scanner scan;
	
	public static String lerString() {
	     scan = new Scanner(in);
	     boolean ok = false; 
	     String txt = "";
	     while(!ok) {
	         try {
	             txt = scan.nextLine();
	             ok = true;
	         }
	         catch(InputMismatchException e) 
	             { out.println("Texto Inválido"); 
	               out.print("Novo valor: ");
	               scan.nextLine(); 
	             }
	     }
	     
         //input.close();
	     return txt;
	  } 

	 
	 public static int lerInt() {
	     scan = new Scanner(in);
	     boolean ok = false; 
	     int i = 0; 
	     while(!ok) {
	         try {
	             i = scan.nextInt();
	             ok = true;
	         }
	         catch(InputMismatchException e) 
	             { out.println("Inteiro Inválido"); 
	               out.print("Novo valor: ");
	               scan.nextLine(); 
	             }
	     }
	     //input.close();
	     return i;
	  } 
	  
	  public static double lerDouble() {
	     scan = new Scanner(in);
	     boolean ok = false; 
	     double d = 0.0; 
	     while(!ok) {
	         try {
	             d = scan.nextDouble();
	             ok = true;
	         }
	         catch(InputMismatchException e) 
	             { out.println("Valor real Inválido"); 
	               out.print("Novo valor: ");
	               scan.nextLine(); 
	             }
	     }
	     //input.close();
	     return d;
	  }  
	  
	   public static float lerFloat() {
	     scan = new Scanner(in);
	     boolean ok = false; 
	     float f = 0.0f; 
	     while(!ok) {
	         try {
	             f = scan.nextFloat();
	             ok = true;
	         }
	         catch(InputMismatchException e) 
	             { out.println("Valor real Inválido"); 
	               out.print("Novo valor: ");
	               scan.nextLine(); 
	             }
	     }
	     //input.close();
	     return f;
	  }  
	  
	   public static boolean lerBoolean() {
	     scan = new Scanner(in);
	     boolean ok = false; 
	     boolean b = false; 
	     while(!ok) {
	         try {
	             b = scan.nextBoolean();
	             ok = true;
	         }
	         catch(InputMismatchException e) 
	             { out.println("Booleano Inválido"); 
	               out.print("Novo valor: ");
	               scan.nextLine(); 
	             }
	     }
	     //input.close();
	     return b;
	  } 
	  
	  public static short lerShort() {
	     scan = new Scanner(in);
	     boolean ok = false; 
	     short s = 0; 
	     while(!ok) {
	         try {
	             s = scan.nextShort();
	             ok = true;
	         }
	         catch(InputMismatchException e) 
	             { out.println("Short Inválido"); 
	               out.print("Novo valor: ");
	               scan.nextLine(); 
	             }
	     }
	     scan.close();
	     return s;
	  }   

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
        return Input.lerString();
    }
	
	/**
	 * Scans the user for an int value, handling errors
	 * @param message Message to be displayed
	 * @return
	 */
	public static int scanInt(String message) {
        	System.out.println(message);	         
	         return Input.lerInt();
	 }
	
	/**
	 * Scans the user for an int value in a given range, handling errors 
	 * @param message Message to be displayed
	 * @param min Minimum value of the range
	 * @param max Maximum value of the range
	 * @return
	 */
	public static int intInRange(String message, int min, int max) {
        int val = Input.scanInt(message);

        while ( val < min || val > max ) {
            System.out.println("Value is out of bounds\n");
            val = Input.intInRange(message, min, max);
        }

        return val;
    }
	
	/**
	 * Scans the user for a single character, handling errors
	 * @param message Message to be displayed
	 * @return
	 */
	public static char scanChar(String message) {
		String line = Input.scanString(message);
		while( line.equals("") )
			Input.scanString("Invalid value." + message);
		
		return line.charAt(0);
	}
	
}
