/**
 *
 * @author frmendes
 */

import java.util.GregorianCalendar;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.io.Console;

public class FitnessUM {
    
    private UserDatabase users;
    private boolean active;
    private User currentUser;

    // Name regex - names can't contain more than one space and must not contain any numbers
    private static final String NAMEREGEX = "^[[a-z][A-Z]]*\\s?[[a-z][A-Z]]*$";
    // Email regex
    private static final String EMAILREGEX = "\\A[\\w\\-.]+@[a-z\\-\\d]+(\\.[a-z]+)*\\.[a-z]+\\z";

    /** Empty constructor
     */
    public FitnessUM() {
        this.users = new UserDatabase();
        this.currentUser = new User();
    }

    /** Parameterized constructor
     * @param users existing UserDatabase
     */
    public FitnessUM(UserDatabase users) {
        this.users = users.clone();
        this.currentUser = new User();
    }

    /** Copy constructor
     * @param fit existing FitnessUM app
     */
    public FitnessUM(FitnessUM fit) {
        this.users = fit.getUsers();
        this.currentUser = fit.getCurrentUser();
    }

    /** Get user list
     * It will always be run in context of the class itself, for safety reasons
     * @return clone of user database
     */
    private UserDatabase getUsers() {
        return this.users.clone();
    }

    /** Getter for logged in user ID
     * @return logged in user ID
     */
    public User getCurrentUser() {
        return this.currentUser.clone();
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

    /** Scans for information and saves the user into the database
     */
    public void registerUser() {
        String name = FitnessUM.scanName("First name: ") + FitnessUM.scanName("Last name: ");
        String email = FitnessUM.scanEmail();
        
        while ( this.users.findByEmail(email) != null ) {
            System.out.println("Email is already taken");
            email = FitnessUM.scanEmail();
        }
        
        String password = FitnessUM.scanPassword();
        UserInfo info = FitnessUM.scanUserInfo();

        this.users.save( new User(name, password, email, info) );
    }

    /** Scans for valid login info and sets the current_user
     */
    public void loginUser() {
        int nrAttempts = 0;
        boolean logged = false;
        User u = new User();
        
        
        while (nrAttempts < 3 && !logged) {
            u = this.users.findByEmail( FitnessUM.scanString("Enter email:") );
            while ( u == null ) {
                System.out.println("We have no record of that email...");
                u = this.users.findByEmail( FitnessUM.scanString("Enter email:") );
            }
            
            String pw = FitnessUM.scanString("Enter password:");
            
            if ( u.matchPassword(pw) )
                logged = true;
            else
                System.out.println("Password and email don't match. " + (3 - ++nrAttempts) + " attempts remaining.");
            
        }
        
        if (logged)
            this.currentUser = u.clone();
        else {
            System.out.println("Too many failed attempts. We've called the cops.\nBye bye.");
            this.shutdown();
        }
    }

    /** Reads an integer from the user input and starts up or shuts down the app accordingly
     */
    public void getStartOption() {
        System.out.println("Choose one of the following options\n1. Login\n2. Register\n0. Exit\n");
        int option = FitnessUM.scanIntInRange(0, 2);
        
        switch(option) {
            case 1:
                this.startup();
                this.loginUser();
                break;
            case 2:
                this.startup();
                this.registerUser();
                break;
            default:
                this.shutdown();
                break;
        }        
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
    private static int scanIntInRange(int min, int max) {
        Scanner scan = new Scanner(System.in);
        System.out.println("Please provide a value in [" + min + ", " + max + "]");
        int val = scan.nextInt();

        while ( val < min && val > max ) {
            System.out.println("Invalid value");
            val = FitnessUM.scanIntInRange(min, max);
        }

        return val;
    }

    /** Scans the user for gender, height, weight, birth date and favorite sport
     * @return u UserInfo containing scanned information
     */
    private static UserInfo scanUserInfo() {
        UserInfo u = new UserInfo();
        u.setGender( FitnessUM.scanGender() );
        u.setHeight( FitnessUM.scanDouble("How tall are you? (cm) ") );
        u.setWeight( FitnessUM.scanDouble("What's your current weight? (kg)") );
        u.setBirthDate( FitnessUM.scanDate("When where you born?") );
        u.setFavoriteSport( FitnessUM.scanString("Share your favorite sport.") );

        return u;
    }

    /** Scans the user for a date
     * @param message message for print
     * @return date Date given
     */
    private static GregorianCalendar scanDate(String message) {
        System.out.println(message);
        int day = Integer.parseInt( FitnessUM.scanString("Enter the day:") );
        int month = Integer.parseInt( FitnessUM.scanString("Enter the month:") );
        int year = Integer.parseInt( FitnessUM.scanString("Enter the year:") );
        
        GregorianCalendar date = new GregorianCalendar(year, month - 1, day);
        date.setLenient(false);
        try {
            date.getTime();
        } catch (Exception e) {
            System.out.println("Invalid date");
            date = FitnessUM.scanDate(message);
        }
        
        return date;        
    }
    
    /** Scans the user for a double, presenting a message
     * @param message message message to be print
     * @return d scanned double
     */    
    private static double scanDouble(String message) {
        System.out.println(message);
        double d;
        try {
            d = new Scanner(System.in).nextDouble();
        } catch (Exception e) {
            System.out.println("Invalid input");
            d = FitnessUM.scanDouble(message);
        }
        return d;
    }

    /** Scans the user for gender
     * @return boolean corresponding to the gender
     */
    private static boolean scanGender() {
        boolean g;
        String gender = FitnessUM.scanString("Are you male or female?");

        switch ( gender.trim().toLowerCase() ) {
            case "male":
                g = true;
                break;
                
            case "female":
                g = false;
                break;
                
            default:
                System.out.println("Invalid option");
                g = FitnessUM.scanGender();
                break;
        }

        return g;
    }

    /** Scans the user for a string
     * @param message message to be displayed
     * @return scanned message
     */
    private static String scanString(String message) {
        System.out.println(message);
        return new Scanner(System.in).nextLine();
    }

    /** Scans the user for a password
     * @return user password
     */
    private static String scanPassword() {
        String pw = FitnessUM.consolePassword();
        
        if ( pw == null )
            pw = FitnessUM.idePassword();
        
        return pw;
    }
    
    /** Scans the user for a email
     * @return user email
     */
    private static String scanEmail() {
        String email = FitnessUM.scanString("Enter email:");

        while ( !FitnessUM.matchEmail(email) )
            email = FitnessUM.scanString("Invalid e-mail\nEnter email:");

        return email;
    }

    /** Scans the user for a first and last name
     * First and last names cannot contain more than one space each or numbers
     * @param message to be displayed
     * @return string containing both names concatenated
     */
    private static String scanName(String message) {
        String name = FitnessUM.scanString(message);

        while( !FitnessUM.matchName(name) )
            name = FitnessUM.scanString("Invalid input\n" + message);

        return name;
    }
    
    private static String idePassword() {
        String pw = FitnessUM.scanString("Enter password:");
        
        while ( pw.length() < 8 )
            pw = FitnessUM.scanString("Password has to be at least 8 characters long\nEnter password:");
        
        String pwConfirmation = FitnessUM.scanString("Re-type Password:");
        
        if( pw.equals(pwConfirmation) )
            return pw;
        else {
            System.out.println("Passwords don't match");
            return FitnessUM.scanPassword(); // I love recursion for error handling
        }
    }
    
    private static String consolePassword() {
        String pw = FitnessUM.consoleScanPassword("Enter password:");
        
        // Ugly, but needed. IDEs and System.console don't like to mix
        if (pw == null)
            return null;
        
        while( pw.length() < 8)
            pw = FitnessUM.consoleScanPassword("Password has to be at least 8 characters long\nEnter password:");
        
        String pwConfirmation = FitnessUM.consoleScanPassword("Re-type Password:");
        
        if( pw.equals(pwConfirmation) )
            return pw;
        else {
            System.out.println("Passwords don't match");
            return FitnessUM.consolePassword();
        }
    }

    private static String consoleScanPassword(String message) {
        Console console = System.console();
        
        // Ugly, but needed. IDEs and System.console don't like to mix
        if (console == null)
            return null;
        
        return new String( console.readPassword(message) );
    }
    
    /** Tests if string matches email format
     */
    private static boolean matchEmail(String email) {
        return Pattern.compile(FitnessUM.EMAILREGEX).matcher(email).matches();
    }

    /** Tests if string matches name format
     */
    private static boolean matchName(String name) {
        return Pattern.compile(FitnessUM.NAMEREGEX).matcher(name).matches();
    }

    public static void main(String[] args) {
        new FitnessUM().run();
    }
}
