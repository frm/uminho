
import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.GregorianCalendar;

/**
 *
 * @author joaorodrigues
 */
public abstract class Activity implements Serializable {
  private int calories;
  private GregorianCalendar date;
  private long duration;
  
  /** Minimum time to practice an activity (in milliseconds) */
  private static final long minimumTime = 60000;
  
    //constructors

    /**
     *
     */
        public Activity(){
        this.date = new GregorianCalendar();
        this.duration = 0;
        this.calories = 0;
    }
    
    /**
     *
     * @param date
     * @param duration
     */
    public Activity(GregorianCalendar date, long duration) {
        this.date = date;
        this.duration = duration;
    }
    
    /**
     *
     * @param a
     */
    public Activity(Activity a){
        this.date = a.getDate();
        this.duration = a.getDuration();
        this.calories = a.getCalories();
    }
  
    //setters
    
    /**
     *
     * @param date
     */
        
    public void setDate(GregorianCalendar date) {
        this.date = (GregorianCalendar) date.clone();
    }
    
    /**
     *
     * @param duration
     */
    public void setDuration(long duration) {
        this.duration = duration;
    }
    
    /**
     *
     * @param calories
     */
    public void setCalories(int calories) {
        this.calories = calories;
    }

    /**
     *
     * @return
     */
    public GregorianCalendar getDate() {
        return (GregorianCalendar) this.date.clone();
    }
    
    /**
     *
     * @return
     */
    public long getDuration() {
        return this.duration;
    }
    
    /**
     *
     * @return
     */
    public int getCalories() {
        return this.calories;
    }
    
    /**
     *
     * @return
     */
    public String getName(){
        return this.getClass().getSimpleName();
    }
    
    /**
     *
     * @return
     */
    public boolean hasMinimumTime() {
        return duration >= Activity.minimumTime;
    }  
    
    /**
     *
     * @param a
     * @return
     */
    public boolean aliasOf(Activity a) {
        long currStartTime = this.date.getTimeInMillis();
        long actStartTime = a.getDate().getTimeInMillis();
        boolean upperAlias = (this.duration + currStartTime >= actStartTime) && (currStartTime <= actStartTime);
        boolean lowerAlias = (a.getDuration() + actStartTime >= currStartTime) && (actStartTime <= currStartTime);
        
        return (upperAlias || lowerAlias);
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
