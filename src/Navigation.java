
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Scanner;

/**
 *
 * @author frmendes
 */
public abstract class Navigation {
    
    private int navigator;
    private boolean end;
    private boolean navigating;
    
    public Navigation() {
        this.navigator = 0;
        this.end = false;
        this.navigating = false;
    }
    
    public abstract void action(Object o);
    public abstract void print(Object o);
    
    public void next(ArrayList<Object> list) {
        int limit = this.navigator + 10;
        Iterator<Object> it = list.iterator();
        
        while ( this.navigator < limit && it.hasNext() ) {
            System.out.print(this.navigator + 1);
            this.print( list.get(this.navigator++) );
        }
        
        if (this.navigator < limit)
            this.end = true;
        
    }
    
    public void backtrace(ArrayList<Object> list) {
        int i = this.navigator - 10;
        
        while ( i < this.navigator ) {
            System.out.print(i + 1);
            this.print( list.get(i++) );
        }
    }
    
    public int navigateIn(ArrayList<Object> list) {
        this.next(list);
        
        while( this.isNavigating() ) {
            if ( this.reachedEnd() )
                this.endOptions();
            if (this.navigator == 10)
                this.startOptions();
            else
                this.normalOptions();
        }
    }
    
    public boolean reachedEnd() {
        return this.end;
    }
    
    public boolean isNavigating() {
        return this.navigating;
    }
    
    public void endOptions() {
        System.out.println("\nNo more options available. Press n to continue, b to backtrace.\nEnter an option number to select it.");
        Navigation.parseOptions();
    }
    
    public static void parseOptions() {
        char c = new Scanner(System.in).nextLine().charAt(0);
        int option;
        if ( Character.isDigit(c) )
            try {
                option = Navigation.parseIntitInRange(this.getMinimumOption(), this.navigator, c);
            } catch {
                
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
