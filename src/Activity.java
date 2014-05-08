
import java.util.GregorianCalendar; 

/*Base Activity(Indoor Activities)
*class chain: Activity -> ActivityWeather -> ActivityDistance -> ActivityAltitude
*/

public class Activity
{
  GregorianCalendar date;
  GregorianCalendar duration;
  int calories;
  
    //constructors
    public Activity(){
        this.date = new GregorianCalendar();
        this.duration = new GregorianCalendar();
        this.calories = 0;
    }
    
    public Activity(GregorianCalendar d1, GregorianCalendar d2, int c) {
        this.date = d1;
        this.duration = d2;
        this.calories = c;
    }
    
    public Activity(Activity a){
        this.date = a.getDate();
        this.duration = a.getDuration();
        this.calories = a.getCalories();
    }
  
    //sets
    void setDate(GregorianCalendar date){this.date = (GregorianCalendar) date.clone();}
    void setDuration(GregorianCalendar duration){this.duration = (GregorianCalendar) duration.clone();}
    void setCalories(int calories){this.calories = calories;}

    //gets
    GregorianCalendar getDate(){return (GregorianCalendar) this.date.clone();}
    GregorianCalendar getDuration(){return (GregorianCalendar) this.duration.clone();}
    int getCalories(){return this.calories;}
  
    public Activity clone(){
       return new Activity(this);
    }
    
    public String toString(){
        StringBuilder result = new StringBuilder();
        result.append("### IndoorActivity: ###");
        result.append("\nDate: ");
        result.append(this.date);
        result.append("\nDuration: ");
        result.append(this.duration);
        result.append("\nCalories spent: ");
        result.append(this.calories);
        
        return result.toString();
    }
    
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Activity a = (Activity) o;
       
       return (a.getDate() == this.date && a.getDuration() == this.duration && a.getCalories() == this.calories);
   }

}