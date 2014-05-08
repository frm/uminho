
import java.util.GregorianCalendar; 

/*Base Activity(Indoor Activities)
  class chain: Activity -> ActivityDistance -> ActivityAltitude
*/

public class Activity
{
  String name;
  String weather;
  GregorianCalendar date;
  GregorianCalendar duration;
  int calories;
  
    //constructors
    public Activity(){
        this.name = "";
        this.weather = "";
        this.date = new GregorianCalendar();
        this.duration = new GregorianCalendar();
        this.calories = 0;
    }
    
    public Activity(String n, String w, GregorianCalendar date, GregorianCalendar duration, int c) {
        this.name = n;
        this.weather = w;
        this.date = date;
        this.duration = duration;
        this.calories = c;
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
    void setWeather(String w){this.weather = w;}
    void setDate(GregorianCalendar date){this.date = (GregorianCalendar) date.clone();}
    void setDuration(GregorianCalendar duration){this.duration = (GregorianCalendar) duration.clone();}
    void setCalories(int calories){this.calories = calories;}

    //getters
    String getName(){return this.name;}
    String getWeather(){return this.weather;}
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
        result.append(this.weather);
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
       
       return ((this.name).equals(a.getName()) && (this.weather).equals(a.getWeather()) && this.date == a.getDate() && this.duration == a.getDuration() && this.calories == a.getCalories());
   }

}