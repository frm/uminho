
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Scanner;

/**
 *
 * @author frmendes
 */
public abstract class Navigation<T> {
    
    private int navigator;
    private boolean end;
    private boolean navigating;
    private ArrayList<T> list;
    
    public Navigation() {
        this.navigator = 0;
        this.end = false;
        this.navigating = false;
        this.list = new ArrayList<T>();
    }
    
    public Navigation(ArrayList<T> list) {
        this.navigator = 0;
        this.end = false;
        this.navigating = false;
        this.list = (ArrayList<T>) list.clone();
    }
    
    public abstract void select(T t);
    public abstract void print(T t);
    public abstract String emptyMessage();
    
    private void next() {
        int limit = this.navigator + 10;
        Iterator<T> it = this.list.iterator();
        
        while ( this.navigator < limit && it.hasNext() ) {
            System.out.print(this.navigator + 1);
            this.print( this.list.get(this.navigator++) );
        }
        
        if (this.navigator < limit)
            this.end = true;
        
        if (this.navigator == 0)
            System.out.println( this.emptyMessage () );
              
    }
    
    private void backtrace() {
        int i = this.navigator - 10;
        
        while ( i < this.navigator ) {
            System.out.print(i + 1);
            this.print( this.list.get(i++) );
        }
    }
    
    public void navigate() {
        this.next();
        
        while( this.isNavigating() ) {
            if ( this.reachedEnd() )
                this.endOptions();
            if (this.navigator == 10)
                this.startOptions();
            else
                this.normalOptions();
        }
    }
    
    private boolean reachedEnd() {
        return this.end;
    }
    
    private boolean isNavigating() {
        return this.navigating;
    }
    
    private void quit() {
        this.navigating = false;
    }
    
    
    private void normalOptions() {
        System.out.println("\nPress c to continue reading, b to backtrace and q to quit.\nEnter an option number to select it");
        this.parseOptions(1);
    }
    
    private void startOptions() {
        System.out.println("\nPress c to continue reading, q to quit.\nEnter an option number to select it.");
        this.parseOptions(2);
    }
    
    private void endOptions() {
        System.out.println("\nNo more options available. Press q to quit, b to backtrace.\nEnter an option number to select it.");
        this.parseOptions(3);
    }
    
    private void parseOptions(int tag) {
        char c = new Scanner(System.in).nextLine().charAt(0);
        if ( Character.isDigit(c) )
            this.actionOptions(tag, c);                    
        else
           this.navigationOptions(1, c);
    }
    
    private void actionOptions(int tag, char c) {
        int option = -1;
        try {
            option = Navigation.parseIntInRange(this.getMinimumOption(), this.navigator, c);
        } catch (Exception e) {
             System.out.println("Invalid option");
             this.parseOptions(tag);
        }
            
            this.select( this.list.get(option - 1) );
    }
    
    private void navigationOptions(int tag, char option) {
        char c = Character.toLowerCase(option);
        if ( tag != 1 && c == 'n')
            this.next();
        else if (tag != 2 &&  c == 'b')
            this.backtrace();
        else if ( c == 'q' )
            this.quit();
        else {
            System.out.println("Invalid option");
            this.parseOptions(tag);
        }
    }
    
    private int getMinimumOption() {
        return Math.max(1, this.navigator - 9);
    }
    
    private static int parseIntInRange(int min, int max, char c) throws IllegalArgumentException {
        int val = Character.getNumericValue(c);
        
        if (val < min || val > max)
            throw new IllegalArgumentException("Value off range");
        
        return val;
    }
}
