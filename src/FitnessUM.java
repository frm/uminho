/**
 *
 * @author frmendes
 */

import java.io.Console;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Scanner;
import java.util.regex.Pattern;

public class FitnessUM {
    
    private boolean active;
    private UserController userController;
    private ActivityController activityController;


   private static final String[] startOptions = { "Exit", "Register", "Login" };
   
   private static final String[] mainOptions = {
       "Logout", "My Profile", "Friend List", "Add New Activity Session"
   };
   
   
    /** Empty constructor
     */
    public FitnessUM() {
        this.userController = new UserController();
        this.activityController = new ActivityController();
    }

    /** Parameterized constructor
     * @param users existing UserDatabase
     */
    public FitnessUM(UserController userController, ActivityController activityController) {
        this.userController = userController.clone();
        this.activityController = activityController.clone();
    }

    /** Copy constructor
     * @param fit existing FitnessUM app
     */
    public FitnessUM(FitnessUM fit) {
        this.userController = fit.getUserController();
        this.activityController = fit.getActivityController();
    }

    /** Getter for logged in user ID
     * @return logged in user ID
     */
    public User getUserCurrentUser() {
        return this.userController.getCurrentUser();
    }
    
    private UserController getUserController() {
        return this.userController.clone();
    }
    
    private ActivityController getActivityController(){
        return this.activityController.clone();
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
        String name = FitnessUM.scanName("\nFirst name: ") + " " + FitnessUM.scanName("\nLast name: ");
        String email = FitnessUM.scanEmail();

        while ( ! this.userController.validateEmailUniqueness(email) ) {
            System.out.println("Email is already taken");
            email = FitnessUM.scanEmail();
        }
        
        String password = FitnessUM.scanPassword();
        UserInfo info = FitnessUM.scanUserInfo();

        this.userController.registerUser(name, email, password, info);
        this.userController.loginUser(email, password);
    }

    /** Scans for valid login info and sets the current_user
     */
    public void loginUser() {
        int nrAttempts = 0;
        boolean logged = false;        
        
        while (nrAttempts < 3 && !logged) {
            String email = FitnessUM.scanString("Enter email:");

            while ( !this.userController.existsUserWithEmail(email) ) {
               System.out.println("We have no record of that email...");
               email = FitnessUM.scanString("Enter email:");
            }
            
            String pw = FitnessUM.scanString("Enter password:");
            
            if ( this.userController.loginUser(email, pw) )
                logged = true;
            else
                System.out.println("Password and email don't match. " + (3 - ++nrAttempts) + " attempt(s) remaining.");
            
        }
        
        if (! logged) {
            System.out.println("Too many failed attempts. We called the cops.\nBye bye.");
            this.shutdown();
        }
        
        System.out.println("\nWelcome "+ this.userController.getCurrentUser().getName() );
    }
    
    public void userProfile() {
        System.out.println( this.userController.currentUserProfile() );
        FitnessUM.pressEnterToContinue();
    }
    
    public void listFriends() {
        new FriendListNavigator( this.userController.getFriendList() ).navigate();
    }
    
    public void addActivitySession() {
        
        /*Falta receber se é distance ou altitude ou nada, usar variavel specialCategories, que diz o numero de categorias extra*/
        /*Falta por o navigator para as atividades e os weathers*/
        /*int calories = FitnessUM.scanInt("How many calories did you burn?");
        GregorianCalendar date = FitnessUM.scanDateWithHours("What's the date of this session? (dd-mm-yyyy)", "At what time did it start? (hh:mm)");
        GregorianCalendar duration = FitnessUM.scanDuration("How long was the session? (hh:mm:ss)");
        
        if(specialCategories <= 1){
            int distance = FitnessUM.scanInt("What was the distance?");
            userController.addActivity(type(navigator), weather(navigator) ,calories, date, duration, distance);
        }
        
        else if(specialCategories <= 2){
            int distance = FitnessUM.scanInt("What was the distance?");
            int altitude = FitnessUM.scanInt("What was the altitude?");
            userController.addActivity(type(navigator), weather(navigator) ,calories, date, duration, distance, altitude);
        }
        
        else userController.addActivity(type(navigator), weather(navigator) ,calories, date, duration);*/
    }

    /** Reads an integer from the user input and starts up or shuts down the app accordingly
     */
    public void getStartOption() {
        this.startup();
        System.out.println("Choose one of the following options.");
        FitnessUM.printStartOptions();
        int option = FitnessUM.scanMenuOption(0, 2);
        this.getStartPrompt()[option].exec();
    }

    /** Reads user input and launches a chain of events accordingly
     */
    public void commandInterpreter() {
        System.out.println( "Choose one of the following options.");
        FitnessUM.printMainOptions();
        int option = FitnessUM.scanMenuOption(0, 3);
        this.getMainPrompt()[option].exec();
    }

    /** Controls the main flow of events.
     */
    public void run() {
        System.out.println("Welcome to FitnessUM");

        this.getStartOption();
        while ( this.isActive() )
            this.commandInterpreter();
    }
    
    private static int scanMenuOption(int min, int max) {
        return scanIntInRange("Please provide a value for the menu.", min, max);
    }
    
    private static int scanInt(String message) {
        Scanner scan = new Scanner(System.in);
        System.out.println(message);
        
        int val;
        
         try {
            val = scan.nextInt();
        } catch (Exception e) {
            System.out.println("Invalid option");
            val = FitnessUM.scanInt(message);
        }
         
         return val;
    }

    /** Scans the system input for a new integer in a given inclusive range.
     * @param min minimum range
     * @param max maximum range
     * @return read value
     */
    private static int scanIntInRange(String message, int min, int max) {
        int val = FitnessUM.scanInt(message);

        while ( val < min || val > max ) {
            System.out.println("Invalid value\n");
            val = FitnessUM.scanIntInRange(message, min, max);
        }

        return val;
    }

    /** Scans the user for gender, height, weight, birth date and favorite sport
     * @return u UserInfo containing scanned information
     */
    private static UserInfo scanUserInfo() {
        UserInfo u = new UserInfo();
        u.setGender( FitnessUM.scanGender() );
        u.setHeight( FitnessUM.scanHeight() );
        u.setWeight( FitnessUM.scanWeight() );
        u.setBirthDate( FitnessUM.scanDate("When were you born? (dd-mm-yyyy)") );
        u.setFavoriteSport( FitnessUM.scanSport() );

        return u;
    }
    
    private static int[] scanDateArray(String message) {
        String[] dateAry= FitnessUM.scanString("\n"+message).split("-");
        
        if ( dateAry.length != 3 ) {
            System.out.println("Invalid date");
            return scanDateArray(message);
        }
        
        int[] date = new int[3];
        
        for (int i = 0; i < 3; i++) {
            
            try {
                date[i] = Integer.parseInt(dateAry[i]);
            } catch (Exception e) {
                System.out.println("Invalid date");
                return scanDateArray(message);
            }
        }
        return date;       
    }
    
    private static int[] scanHourArray(String message) {
        String[] hourAry= FitnessUM.scanString("\n"+message).split(":");
        
        if ( hourAry.length != 2 ) {
            System.out.println("Invalid time");
            return scanHourArray(message);
        }
        
        int[] hour = new int[2];
        
        for (int i = 0; i < 2; i++) {
            
            try {
                hour[i] = Integer.parseInt(hourAry[i]);
            } catch (Exception e) {
                System.out.println("Invalid time");
                return scanHourArray(message);
            }
        }
        return hour;       
    }
    
    private static boolean validDate(GregorianCalendar date){
        GregorianCalendar now = new GregorianCalendar();
        if ( date.get(Calendar.YEAR) < 1900 || date.compareTo(now) != -1 ) // We don't want any dates < 1900 or >= current time
            return false;
        return true;
    }
    
    private static boolean validGregorianCalendar(GregorianCalendar date) {
        
        date.setLenient(false);     // Allows for date verification
        try {
            date.getTime();           // If the date isn't valid, with setLenient(false), GregorianCalendar#getTime() throws an exception
        } catch (Exception e) {
            return false;
        }
        
        return true;
    }
    
    /** Scans the user for a date
     * @return Date given
     */
    private static GregorianCalendar scanDate(String message) {
        int[] numbers = FitnessUM.scanDateArray(message);
        GregorianCalendar date = new GregorianCalendar(numbers[2], numbers[1] - 1, numbers[0]);
        
        if ( FitnessUM.validGregorianCalendar(date) && FitnessUM.validDate(date) )
            return date;
        
        else {
            System.out.println("Invalid date");
            return FitnessUM.scanDate(message);
        }
    }
    
    private static GregorianCalendar scanDateWithHours(String messageForDate, String messageForTime) {
        int[] day = FitnessUM.scanDateArray(messageForDate);
        int[] time = FitnessUM.scanHourArray(messageForTime);
        GregorianCalendar date = new GregorianCalendar(day[2], day[1] - 1, day[0], time[1], time[0]);
        
        if ( FitnessUM.validGregorianCalendar(date) && FitnessUM.validDate(date) )
            return date;
        
        else {
            System.out.println("Invalid date or time");
            return FitnessUM.scanDateWithHours(messageForDate, messageForTime);
        }
    }
    
    private static int[] scanDurationArray(String message) {
        String[] durationAry= FitnessUM.scanString("\n"+message).split(":");
        
        if ( durationAry.length != 3 ) {
            System.out.println("Invalid duration");
            return scanDurationArray(message);
        }
        
        int[] duration = new int[3];
        
        for (int i = 0; i < 3; i++) {
            
            try {
                duration[i] = Integer.parseInt(durationAry[i]);
            } catch (Exception e) {
                System.out.println("Invalid duration");
                return scanDurationArray(message);
            }
        }
        return duration;       
    }
    
    private static GregorianCalendar scanDuration(String message) {
        int[] numbers = FitnessUM.scanDurationArray(message);
        GregorianCalendar date = new GregorianCalendar(1, 1, 1, numbers[2], numbers[1], numbers[0]);
        
        if ( FitnessUM.validGregorianCalendar(date) )
            return date;
        
        else {
            System.out.println("Invalid duration");
            return FitnessUM.scanDuration(message);
        }
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
    
        private static double scanDoubleInRange(double min, double max, String message) {
        double d = scanDouble(message);
        
        if ( d < min || d > max ) {
            System.out.println("Please. Don't lie.");
            d = scanDoubleInRange(min, max, message);
        }
        
        return d;
    }

    /** Scans the user for gender
     * @return boolean corresponding to the gender
     */
    private static boolean scanGender() {
        boolean g;
        String gender = FitnessUM.scanString("\nAre you male or female?");

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
    
    private static double scanWeight() {
        return scanDoubleInRange(0.0, 300.0, "\nWhat's your current weight? (kg)");
    }
    
    private static double scanHeight() {
        return scanDoubleInRange( 20.0, 300.0, "\nHow tall are you? (cm)" );
    }
    
    private static String scanSport() {
        String sport = FitnessUM.scanString("\nShare your favorite sport.").trim();
        
        if ( ! Pattern.compile("[a-zA-Z]*").matcher(sport).matches() ) {
            System.out.println("Invalid sport.");
            return FitnessUM.scanSport();
        }
        
        return sport;            
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
        String email = FitnessUM.scanString("\nEnter email:");

        while ( !UserController.validEmailFormat(email) )
            email = FitnessUM.scanString("Invalid e-mail\nEnter email:");

        return email;
    }

    /** Scans the user for a first and last name
     * First and last names cannot contain more than one space each or numbers
     * @param message to be displayed
     * @return string containing both names concatenated
     */
    private static String scanName(String message) {
        String name = FitnessUM.scanString(message).trim();

        while( !UserController.validNameFormat(name) )
            name = FitnessUM.scanString("Invalid input\n" + message);

        return name;
    }
    
    private static String idePassword() {
        String pw = FitnessUM.scanString("\nEnter password:");
        
        while ( pw.length() < 8 )
            pw = FitnessUM.scanString("Password has to be at least 8 characters long\nEnter password:");
        
        String pwConfirmation = FitnessUM.scanString("\nRe-type Password:");
        
        if( pw.equals(pwConfirmation) )
            return pw;
        else {
            System.out.println("Passwords don't match");
            return FitnessUM.scanPassword(); // I love recursion for error handling
        }
    }
    
    private static String consolePassword() {
        String pw = FitnessUM.consoleScanPassword("\nEnter password:");
        
        // Ugly, but needed. IDEs and System.console don't like to mix
        if (pw == null)
            return null;
        
        while( pw.length() < 8)
            pw = FitnessUM.consoleScanPassword("Password has to be at least 8 characters long\nEnter password:");
        
        String pwConfirmation = FitnessUM.consoleScanPassword("\nRe-type Password:");
        
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
    
    private Prompt[] getStartPrompt() {
        final FitnessUM app = this;
        return new Prompt[] {
            new Prompt() { public void exec() { app.shutdown(); } },
            new Prompt() { public void exec() { app.registerUser();} },
            new Prompt() { public void exec() { app.loginUser();} }
        };
    }
    
    private Prompt[] getMainPrompt() {
        final FitnessUM app = this;
        return new Prompt[] {
            new Prompt() { public void exec() { app.shutdown(); } },
            new Prompt() { public void exec() { app.userProfile(); } },
            new Prompt() { public void exec() { app.listFriends(); }},
            new Prompt() { public void exec() { app.addActivitySession(); } }
        };
    }
    
    
    
    private static void printStartOptions() {
        int i = 0;
        for (String s : FitnessUM.startOptions)
            System.out.println(i++ + ". " + s);
    }
    
    private static void printMainOptions() {
        int i = 0;
        for (String s : FitnessUM.mainOptions)
            System.out.println(i++ + ". " + s);
    }
    
    private static void pressEnterToContinue() {
        Scanner scan = new Scanner(System.in);
            System.out.println("\nPress Enter To Continue");
            
        while ( scan.nextLine().length() > 0 );
    }
    
    public static String dateFormat(GregorianCalendar c) {
        return new SimpleDateFormat("dd/MM/yyyy").format( c.getTime() );
    }
    
    public static void main(String[] args) {
        new FitnessUM().run();
    }
}
