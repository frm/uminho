
import java.io.Console;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Scanner;
import java.util.regex.Pattern;

/**
 *
 * @author frmendes
 */
public class Scan {
    
    private Scanner stream;
    
    public Scan(InputStream stream) {
        this.stream = new Scanner(stream);
    }
    
    public static String dateFormat(GregorianCalendar c) {
        return new SimpleDateFormat("dd/MM/yyyy").format( c.getTime() );
    }
    
    public static void pressEnterToContinue() {
        Scanner scan = new Scanner(System.in);
        System.out.println("\nPress Enter To Continue");
            
        while ( scan.nextLine().length() > 0 );
    }
    
    public static int menuOption(int min, int max) {
        return Scan.intInRange("Please provide a value for the menu.", min, max);
    }
    
    public static int scanInt(String message) {
        Scanner scan = new Scanner(System.in);
        System.out.println(message);
        
        int val;
        
         try {
            val = scan.nextInt();
        } catch (Exception e) {
            System.out.println("Invalid option");
            val = Scan.scanInt(message);
        }
         
         return val;
    }

    /** Scans the system input for a new integer in a given inclusive range.
     * @param message message to be print
     * @param min minimum range
     * @param max maximum range
     * @return read value
     */
    public static int intInRange(String message, int min, int max) {
        int val = Scan.scanInt(message);

        while ( val < min || val > max ) {
            System.out.println("Invalid value\n");
            val = Scan.intInRange(message, min, max);
        }

        return val;
    }

    /** Scans the user for gender, height, weight, birth date and favorite sport
     * @return u UserInfo containing scanned information
     */
    public static UserInfo userInfo() {
        UserInfo u = new UserInfo();
        u.setGender( Scan.gender() );
        u.setHeight( Scan.height() );
        u.setWeight( Scan.weight() );
        u.setBirthDate( Scan.date("When were you born? (dd-mm-yyyy)") );
        u.setFavoriteSport( Scan.sport() );

        return u;
    }
    
    private static int[] dateArray(String message) {
        String[] dateAry= Scan.scanString("\n"+message).split("-");
        
        if ( dateAry.length != 3 ) {
            System.out.println("Invalid date");
            return Scan.dateArray(message);
        }
        
        int[] date = new int[3];
        
        for (int i = 0; i < 3; i++) {
            
            try {
                date[i] = Integer.parseInt(dateAry[i]);
            } catch (NumberFormatException e) {
                System.out.println("Invalid date");
                return Scan.dateArray(message);
            }
        }
        return date;       
    }
    
    private static int[] hourArray(String message) {
        String[] hourAry= Scan.scanString("\n"+message).split(":");
        
        if ( hourAry.length != 2 ) {
            System.out.println("Invalid time");
            return Scan.hourArray(message);
        }
        
        int[] hour = new int[2];
        
        for (int i = 0; i < 2; i++) {
            
            try {
                hour[i] = Integer.parseInt(hourAry[i]);
            } catch (NumberFormatException e) {
                System.out.println("Invalid time");
                return Scan.hourArray(message);
            }
        }
        return hour;       
    }
    
    public static boolean validDate(GregorianCalendar date){
        GregorianCalendar now = new GregorianCalendar();
        return date.get(Calendar.YEAR) >= 1900 && date.compareTo(now) == -1;
    }
    
    public static boolean validGregorianCalendar(GregorianCalendar date) {
        
        date.setLenient(false);     // Allows for date verification
        try {
            date.getTime();           // If the date isn't valid, with setLenient(false), GregorianCalendar#getTime() throws an exception
        } catch (Exception e) {
            return false;
        }
        
        return true;
    }
    
    /** Scans the user for a date
     * @param message Message to be print
     * @return Date given
     */
    public static GregorianCalendar date(String message) {
        int[] numbers = Scan.dateArray(message);
        GregorianCalendar date = new GregorianCalendar(numbers[2], numbers[1] - 1, numbers[0]);
        
        if ( Scan.validGregorianCalendar(date) && Scan.validDate(date) )
            return date;
        
        else {
            System.out.println("Invalid date");
            return Scan.date(message);
        }
    }
    
    public static GregorianCalendar dateWithHours(String messageForDate, String messageForTime) {
        int[] day = Scan.dateArray(messageForDate);
        int[] time = Scan.hourArray(messageForTime);
        GregorianCalendar date = new GregorianCalendar(day[2], day[1] - 1, day[0], time[1], time[0]);
        
        if ( Scan.validGregorianCalendar(date) && Scan.validDate(date) )
            return date;
        
        else {
            System.out.println("Invalid date or time");
            return Scan.dateWithHours(messageForDate, messageForTime);
        }
    }
    
    private static int[] durationArray(String message) {
        String[] durationAry= Scan.scanString("\n"+message).split(":");
        
        if ( durationAry.length != 3 ) {
            System.out.println("Invalid duration");
            return Scan.durationArray(message);
        }
        
        int[] duration = new int[3];
        
        for (int i = 0; i < 3; i++) {
            
            try {
                duration[i] = Integer.parseInt(durationAry[i]);
            } catch (NumberFormatException e) {
                System.out.println("Invalid duration");
                return Scan.durationArray(message);
            }
        }
        return duration;       
    }
    
    public static GregorianCalendar duration(String message) {
        int[] numbers = Scan.durationArray(message);
        GregorianCalendar date = new GregorianCalendar(1, 1, 1, numbers[2], numbers[1], numbers[0]);
        
        if ( Scan.validGregorianCalendar(date) )
            return date;
        
        else {
            System.out.println("Invalid duration");
            return Scan.duration(message);
        }
    }
    
    
    
    /** Scans the user for a double, presenting a message
     * @param message message message to be print
     * @return d scanned double
     */    
    public static double scanDouble(String message) {
        System.out.println(message);
        double d;
        try {
            d = new Scanner(System.in).nextDouble();
        } catch (Exception e) {
            System.out.println("Invalid input");
            d = Scan.scanDouble(message);
        }
        return d;
    }
    
    public static double doubleInRange(double min, double max, String message) {
        double d = Scan.scanDouble(message);
        
        if ( d < min || d > max ) {
            System.out.println("Please. Don't lie.");
            d = Scan.doubleInRange(min, max, message);
        }
        
        return d;
    }

    /** Scans the user for gender
     * @return boolean corresponding to the gender
     */
    public static boolean gender() {
        boolean g;
        String gender = Scan.scanString("\nAre you male or female?");

        switch ( gender.trim().toLowerCase() ) {
            case "male":
                g = true;
                break;
                
            case "female":
                g = false;
                break;
                
            default:
                System.out.println("Invalid option");
                g = Scan.gender();
                break;
        }

        return g;
    }
    
    public static double weight() {
        return Scan.doubleInRange(0.0, 300.0, "\nWhat's your current weight? (kg)");
    }
    
    public static double height() {
        return Scan.doubleInRange( 20.0, 300.0, "\nHow tall are you? (cm)" );
    }
    
    public static String sport() {
        String sport = Scan.scanString("\nShare your favorite sport.").trim();
        
        if ( ! Pattern.compile("^[\\p{L} ]*$").matcher(sport).matches() ) {
            System.out.println("Invalid sport.");
            return Scan.sport();
        }
        
        return sport;            
    }
    /** Scans the user for a string
     * @param message message to be displayed
     * @return scanned message
     */
    public static String scanString(String message) {
        System.out.println(message);
        return new Scanner(System.in).nextLine();
    }

    /** Scans the user for a password
     * @return user password
     */
    public static String password() {
        String pw = Scan.consolePassword();
        
        if ( pw == null )
            pw = Scan.idePassword();
        
        return pw;
    }
    
    /** Scans the user for a email
     * @return user email
     */
    public static String email() {
        String email = Scan.scanString("\nEnter email:");

        while ( !UserController.validEmailFormat(email) )
            email = Scan.scanString("Invalid e-mail\nEnter email:");

        return email;
    }

    /** Scans the user for a first and last name
     * First and last names cannot contain more than one space each or numbers
     * @param message to be displayed
     * @return string containing both names concatenated
     */
    public static String name(String message) {
        String name = Scan.scanString(message).trim();

        while( !UserController.validNameFormat(name) )
            name = Scan.scanString("Invalid input\n" + message);

        return name;
    }
    
    private static String idePassword() {
        String pw = Scan.scanString("\nEnter password:");
        
        while ( pw.length() < 8 )
            pw = Scan.scanString("Password has to be at least 8 characters long\nEnter password:");
        
        String pwConfirmation = Scan.scanString("\nRe-type Password:");
        
        if( pw.equals(pwConfirmation) )
            return pw;
        else {
            System.out.println("Passwords don't match");
            return Scan.password(); // I love recursion for error handling
        }
    }
    
    private static String consolePassword() {
        String pw = Scan.consoleScanPassword("\nEnter password:");
        
        // Ugly, but needed. IDEs and System.console don't like to mix
        if (pw == null)
            return null;
        
        while( pw.length() < 8)
            pw = Scan.consoleScanPassword("Password has to be at least 8 characters long\nEnter password:");
        
        String pwConfirmation = Scan.consoleScanPassword("\nRe-type Password:");
        
        if( pw.equals(pwConfirmation) )
            return pw;
        else {
            System.out.println("Passwords don't match");
            return Scan.consolePassword();
        }
    }

    private static String consoleScanPassword(String message) {
        Console console = System.console();
        
        // Ugly, but needed. IDEs and System.console don't like to mix
        if (console == null)
            return null;
        
        return new String( console.readPassword(message) );
    } 
    
}
