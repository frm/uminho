
import java.util.GregorianCalendar; 

/*Base Activity(Indoor Activities)
  class chain: Activity -> ActivityDistance -> ActivityAltitude
*/

public class Activity
{
  String name;
  int weather;
  GregorianCalendar date;
  GregorianCalendar duration;
  int calories;
  
  private static final String[] weatherStates = {"Indoor", "Clear Sky", "Cloudy", "Light Rain", "Heavy Rain", "Snowing", "Hailing", "Windy", "Stormy"};
  
    //constructors
    public Activity(){
        this.name = "";
        this.weather = -1;
        this.date = new GregorianCalendar();
        this.duration = new GregorianCalendar();
        this.calories = 0;
    }
    
    public Activity(String name, int weather, GregorianCalendar date, GregorianCalendar duration, int calories) {
        this.name = name;
        this.weather = weather;
        this.date = date;
        this.duration = duration;
        this.calories = calories;
    }
    
    public Activity(Activity a){
        this.name = a.getName();
        this.weather = a.getWeather();
        this.date = a.getDate();
        this.duration = a.getDuration();
        this.calories = a.getCalories();
    }
  
    //setters
    void setName(String n){this.name = n;}
    void setWeather(int w){this.weather = w;}
    void setDate(GregorianCalendar date){this.date = (GregorianCalendar) date.clone();}
    void setDuration(GregorianCalendar duration){this.duration = (GregorianCalendar) duration.clone();}
    void setCalories(int calories){this.calories = calories;}

    //getters
    String getName(){return this.name;}
    int getWeather(){return this.weather;}
    String getWeatherState(){return weatherStates[this.weather];}
    GregorianCalendar getDate(){return (GregorianCalendar) this.date.clone();}
    GregorianCalendar getDuration(){return (GregorianCalendar) this.duration.clone();}
    int getCalories(){return this.calories;}
  
    //essentials
    public Activity clone(){
       return new Activity(this);
    }
    
    public String toString(){
        StringBuilder result = new StringBuilder();
        result.append("### Activity: ###");
        result.append("\nName: ");
        result.append(this.name);
        result.append("\nDate: ");
        result.append(this.date);
        result.append("\nWeather: ");
        result.append(weatherStates[this.weather]);
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
       
       return ((this.name).equals(a.getName()) && this.weather == a.getWeather() && this.date == a.getDate() && this.duration == a.getDuration() && this.calories == a.getCalories());
   }

}
