/**
 *
 * @author frmendes
 */

import java.util.Scanner;

public class FitnessUM {
    UserDatabase users;
    boolean active;

    /**
     * Empty constructor
     */
    public FitnessUM() {
        this.users = new UserDatabase();
    }

    /**
     * Copy constructor
     * @param users existing UserDatabase
     */
    public FitnessUM(UserDatabase users) {
        this.users = users.clone();
    }
    
    /** Getter for active variable
     * @return active variable
     */
    public boolean isActive() {
        return this.active;
    }
    
    /** Setter for active variable
     */
    public void startup() {
        this.active = true;
    }
    
    /** Setter for active variable
     */
    public void shutdown() {
        this.active = false;
    }
    
    /** Reads an integer from the user input and starts up or shuts down the app accordingly
     */
    public void getStartOption() {
        System.out.println("Choose one of the following options\n1. Login\n0. Exit\n");
        if ( FitnessUM.scanIntInRange(0, 1) == 1 )
            this.startup();
        else
            this.shutdown();            
    }    
    
    /** Reads user input and launches a chain of events accordingly 
     */
    public void commandInterpreter() {
        System.out.println("IT WORKS!");
        this.getStartOption();
    }
    
    /** Controls the main flow of events.
     */
    public void run() {
        System.out.println("Welcome to FitnessUM");
        
        this.getStartOption();
        while ( this.isActive() )
            this.commandInterpreter();
    }
    
    /** Scans the system input for a new integer in a given inclusive range.
     * Throws IllegalArgumentException if value is off limits.
     * @param min minimum range
     * @param max maximum range
     * @return read value
     */
    public static int scanIntInRange(int min, int max) {
        Scanner scan = new Scanner(System.in);
        int val = scan.nextInt();
        
        if (val < min || val > max)
            throw new IllegalArgumentException("Read value out of bounds");
        
        return val;
    }
    
    public static void main(String[] args) {
        //new FitnessUM().run();
    }
}
