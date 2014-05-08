import java.util.GregorianCalendar; 

public class ActivityDistance extends ActivityWeather{
    int distance;
    
    //constructors
    public ActivityDistance(){
        super();
        this.distance = 0;
    }
    
    public ActivityDistance(GregorianCalendar d1, GregorianCalendar d2, int c, String w, int d3){
        super();
        this.distance = d3;
    }
    
    public ActivityDistance(ActivityDistance ad){
        super();
        this.distance = ad.getDistance();
    }
    
    //sets
    void setDistance(int d){this.distance = d;}
    
    //gets
    int getDistance(){return this.distance;}
    
    //essentials
    public ActivityDistance clone(){
       return new ActivityDistance(this);
    }
    
    public String toString(){
        StringBuilder result = new StringBuilder();
        result.append("### OutdoorActivity(+distance): ###");
        result.append("\nDate: ");
        result.append(this.date);
        result.append("\nDuration: ");
        result.append(this.duration);
        result.append("\nCalories spent: ");
        result.append(this.calories);
        result.append("\n");
        result.append(this.weather);
        result.append("\nDistance: ");
        result.append(this.distance);
        
        return result.toString();
    }
    
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        ActivityDistance ad = (ActivityDistance) o;
       
       return (ad.getDate() == this.date && ad.getDuration() == this.duration && ad.getCalories() == this.calories && ad.getWeather().equals(this.weather) && ad.getDistance() == this.distance);
   }
}
