
import java.text.SimpleDateFormat;
import java.util.ArrayList; 
import java.util.GregorianCalendar;

/*Base Activity(Indoor Activities)
  class chain: Activity -> ActivityDistance -> ActivityAltitude
*/

public class Activity
{
  private int calories;
  private String name;
  private Weather weather;
  private GregorianCalendar date;
  private GregorianCalendar duration;
  
    //constructors
    public Activity(){
        this.name = "";
        this.weather = new Weather();
        this.date = new GregorianCalendar();
        this.duration = new GregorianCalendar();
        this.calories = 0;
    }
    
    public Activity(String name, int weather, GregorianCalendar date, GregorianCalendar duration, int calories) {
        this.name = name;
        this.weather = new Weather(weather);
        this.date = date;
        this.duration = duration;
        this.calories = calories;
    }
    
    public Activity(Activity a){
        this.name = a.getName();
        this.weather = a.getWeatherState();
        this.date = a.getDate();
        this.duration = a.getDuration();
        this.calories = a.getCalories();
    }
  
    //setters
    void setName(String n) {
        this.name = n;
    }
    
    void setWeather(int w) {
        this.weather.setWeather(w);
    }
    
    void setDate(GregorianCalendar date) {
        this.date = (GregorianCalendar) date.clone();
    }
    
    void setDuration(GregorianCalendar duration) {
        this.duration = (GregorianCalendar) duration.clone();
    }
    
    void setCalories(int calories) {
        this.calories = calories;
    }

    //getters
    String getName() {
        return this.name;
    }
    
    String getWeather() {
        return this.weather.getWeather();
    }
    
    Weather getWeatherState() {
        return this.weather.clone();
    }
    GregorianCalendar getDate() {
        return (GregorianCalendar) this.date.clone();
    }
    
    GregorianCalendar getDuration() {
        return (GregorianCalendar) this.duration.clone();
    }
    
    int getCalories() {
        return this.calories;
    }
  
    //essentials
    public Activity clone(){
       return new Activity(this);
    }
    
    public String toString(){
        StringBuilder result = new StringBuilder();

        result.append("\n\nSport: ");
        result.append(this.name);
        result.append("\nDate: ");
        result.append(new SimpleDateFormat("dd/MM/yyyy 'at' HH:mm").format( this.date.getTime() ));
        result.append("\nWeather: ");
        result.append(this.weather);
        result.append("\nDuration: ");
        result.append(new SimpleDateFormat("HH 'hours' mm 'minutes and' ss 'seconds' ").format( this.date.getTime() ));
        result.append("\nCalories burned: ");
        result.append(this.calories);
        
        return result.toString();
    }
    
    public boolean equals(Object o) {
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Activity a = (Activity) o;
       
       return ( this.name.equals( a.getName() ) && this.weather.equals( a.getWeatherState() ) && this.date.equals( a.getDate() ) && this.duration.equals( a.getDuration() ) && this.calories == a.getCalories() );
    }

}
