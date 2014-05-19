
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Scanner;

/**
 *
 * @author frmendes
 */
public abstract class Navigator<T> {
    
    private int navigator;
    private boolean navigating;
    private ArrayList<T> list;
    private static Prompt[] optionsParser;
    
    private static final int NumberDisplays = 10;
    
    public Navigator() {
        this.navigator = 0;
        this.navigating = false;
        this.list = new ArrayList<T>();
        this.optionsParser = Navigator.optionsGenerator(this);
    }
    
    public Navigator(ArrayList<T> list) {
        this.navigator = 0;
        this.navigating = false;
        this.list = (ArrayList<T>) list.clone();
        this.optionsParser = Navigator.optionsGenerator(this);
    }
        
    public abstract void select(T t);
    public abstract void print(T t);
    public abstract String emptyMessage();
    
    private boolean reachedEnd() {
        return this.navigator == this.list.size();
    }
    
    private boolean isNavigating() {
        return this.navigating;
    }
    
    private void startNavigating() {
        this.navigating = true;
    }
    
    private void quit() {
        this.navigating = false;
    }
    
    private int totalElements() {
        return this.list.size();
    }
    
    public void navigate() {
        this.startNavigating();
        this.next();
        
        while( this.isNavigating() ) {
            if ( this.reachedEnd() )
                if ( this.navigator <= Navigator.NumberDisplays )      this.optionsParser[0].exec();
                else                                                                                 this.optionsParser[2].exec();
            else
                if ( this.navigator == Navigator.NumberDisplays )      this.optionsParser[1].exec();
                else                                                                                 this.optionsParser[3].exec();
        }
    }
    
    private void next() {        
        int limit = Math.min(this.navigator + Navigator.NumberDisplays, this.totalElements() );

        this.printNext(limit);
        
        if (this.navigator == 0) {
            System.out.println( this.emptyMessage() );
            Scan.pressEnterToContinue();
            this.quit();
        }
              
    }
    
    private void reprint() {
        Scan.pressEnterToContinue();
        int limit = this.navigator;
        this.rewindNavigator();
        
        this.printNext(limit);        
    }
    
    private void backtrace() {
        this.rewindNavigator();
        int limit = this.navigator;
        this.navigator -= Navigator.NumberDisplays;
        
        this.printNext(limit);
    }
    
    private void rewindNavigator() {
        do
            this.navigator--;
        while (this.navigator % Navigator.NumberDisplays != 0);
    }
    
    private void printNext(int limit) {
        System.out.println("\n");
        while ( this.navigator < limit) {
            System.out.print(this.navigator + 1 + ". ");
            this.print( list.get(this.navigator++) );
        }
    }
    
    
    private void invalidOption(int permission) {
        System.out.println("Invalid option");
        this.optionsParser[permission].exec();
    }

    private void parseOptionsPermit(int permission) {
        String s = new Scanner(System.in).nextLine() ;
        if (s.length() == 0) {
            this.invalidOption(permission);
            return;
        }
        
        char c = s.charAt(0);
        
        if ( Character.isDigit(c) )
            this.actionOptions(permission, s);                    
        else
           this.navigationOptions(permission, c);
    }
    
    private void actionOptions(int permission, String s) {
        int option = -1;
        try {
            option = Navigator.parseIntInRange(this.getMinimumOption(), this.list.size(), s);
        } catch (IllegalArgumentException e) {
            this.invalidOption(permission);
            return;
        }
            
            this.select( this.list.get(option - 1) );
            this.reprint();
    }
    
    private void navigationOptions(int permission, char option) {
        char c = Character.toLowerCase(option);
        switch (c) {
                case 'q':
                    this.quit();
                    break;
                case 'c':
                    if(permission == 1 || permission == 3) this.next();
                    else this.invalidOption(permission);
                    break;
                case 'b':
                        if (permission == 2 || permission == 3) this.backtrace();
                        else this.invalidOption(permission);
                        break;
                default:
                    this.invalidOption(permission);
                    break;                    
        }
    }
    
    private int getMinimumOption() {
        return Math.max(1, this.navigator - Navigator.NumberDisplays - 1);
    }
    
    // For a few seconds I felt like I was writing JavaScript
    private static Prompt[] optionsGenerator(final Navigator n) {
        return new Prompt[] {
            new Prompt() { public void exec() {
                System.out.println("\nNo more options available. Press 'q' to quit.\nEnter an option number to select it.");
                n.parseOptionsPermit(0);
            }},
             new Prompt() { public void exec() {
                System.out.println("\nPress 'c' to continue reading, 'q' to quit.\nEnter an option number to select it.");
                n.parseOptionsPermit(1);
              }},
             new Prompt() { public void exec() {
                 System.out.println("\nNo more options available. Press 'q' to quit, 'b' to backtrace.\nEnter an option number to select it.");
                 n.parseOptionsPermit(2);
             }},
             new Prompt() { public void exec() {
                System.out.println("\nPress 'c' to continue reading, 'b' to backtrace and 'q' to quit.\nEnter an option number to select it");
                n.parseOptionsPermit(3);
             }}                
        };
    }
        
    private static int parseIntInRange(int min, int max, String s) throws IllegalArgumentException {
        int val = Integer.parseInt(s);
        
        if (val < min || val > max)
            throw new IllegalArgumentException("Value off range");
        
        return val;
    }
}
