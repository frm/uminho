
import java.util.GregorianCalendar;


public class ActivityWeather extends Activity{
    String weather;
    
    //constructors
    public ActivityWeather(){
        super();
        this.weather = "";
    }
    
    public ActivityWeather(GregorianCalendar d1, GregorianCalendar d2, int c, String w){
        super();
        this.weather = w;
    }
    
    public ActivityWeather(ActivityWeather aw){
        super();
        this.weather = aw.getWeather();
    }
    
    //sets
    void setWeather(String w){this.weather = w;}
    
    //gets
    String getWeather(){return this.weather;}
    
    //essentials
    public ActivityWeather clone(){
       return new ActivityWeather(this);
    }
    
    public String toString(){
        StringBuilder result = new StringBuilder();
        result.append("### OutdoorActivity: ###");
        result.append("\nDate: ");
        result.append(this.date);
        result.append("\nDuration: ");
        result.append(this.duration);
        result.append("\nCalories spent: ");
        result.append(this.calories);
        result.append("\n");
        result.append(this.weather);
        
        return result.toString();
    }
    
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        ActivityWeather aw = (ActivityWeather) o;
       
       return (aw.getDate() == this.date && aw.getDuration() == this.duration && aw.getCalories() == this.calories && aw.getWeather().equals(this.weather));
   }
}
