
import java.text.SimpleDateFormat;
import java.util.GregorianCalendar;

/*Base Activity(Indoor Activities)
  class chain: Activity -> ActivityDistance -> ActivityAltitude
*/

public abstract class Activity
{
  private double calories;
  private GregorianCalendar date;
  private long duration;
  
    //constructors
    public Activity(){
        this.date = new GregorianCalendar();
        this.duration = 0;
        this.calories = 0;
    }
    
    public Activity(GregorianCalendar date, long duration) {
        this.date = date;
        this.duration = duration;
    }
    
    public Activity(Activity a){
        this.date = a.getDate();
        this.duration = a.getDuration();
        this.calories = a.getCalories();
    }
  
    //setters
    
    void setDate(GregorianCalendar date) {
        this.date = (GregorianCalendar) date.clone();
    }
    
    void setDuration(long duration) {
        this.duration = duration;
    }
    
    void setCalories(int calories) {
        this.calories = calories;
    }


    GregorianCalendar getDate() {
        return (GregorianCalendar) this.date.clone();
    }
    
    long getDuration() {
        return this.duration;
    }
    
    double getCalories() {
        return this.calories;
    }
    
    String getName(){
        return this.getClass().getSimpleName();
    }
    
    void setCalories(double calories){
        this.calories = calories;
    }
    
 
    public abstract Activity clone();
    
    public String toString(){
        StringBuilder result = new StringBuilder();
        result.append("\nActivity: ");
        result.append(this.getName());
        result.append("\nDate: ");
        result.append(new SimpleDateFormat("dd/MM/yyyy 'at' HH:mm").format( this.date.getTime() ));
        result.append("\nDuration: ");
        result.append(StatEntry.formatMillis(duration));
        result.append("\nCalories burned: ");
        result.append(this.calories+"\n");
        
        return result.toString();
    }
    
    public boolean equals(Object o) {
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Activity act = (Activity) o;
       
       return (this.date.equals( act.getDate() ) && this.duration == act.getDuration() && this.calories == act.getCalories() );
    }

}
