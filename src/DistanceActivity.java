import java.util.GregorianCalendar; 

public abstract class DistanceActivity extends Activity{
    int distance;
    
    //constructors
    public DistanceActivity(){
        super();
        this.distance = 0;
    }
    
    public DistanceActivity(GregorianCalendar date, long duration, int distance) {
        super(date, duration);
        this.distance = distance;
    }
    
    public DistanceActivity(DistanceActivity ad) {
        super(ad);
        this.distance = ad.getDistance();
    }
    
    //setters
    void setDistance(int d) {
        this.distance = d;
    }
    
    //getters
    int getDistance() {
        return this.distance;
    }
    
    public String toString(){
        StringBuilder understring = new StringBuilder();
        understring.append("\nDistance: ");
        understring.append(this.distance);
        
        return super.toString() + understring.toString();
    }
    
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        DistanceActivity ad = (DistanceActivity) o;
       
       return ( super.equals(o) && this.distance == ad.getDistance() );
   }
}

/*
import java.util.GregorianCalendar; 

public class DistanceActivity extends Activity{
    int distance;
    
    //constructors
    public DistanceActivity(){
        super();
        this.distance = 0;
    }
    
    public DistanceActivity(String name, int weather, GregorianCalendar date, GregorianCalendar duration, int calories, int distance) {
        super(name, weather, date, duration, calories);
        this.distance = distance;
    }
    
    public DistanceActivity(DistanceActivity ad) {
        super(ad);
        this.distance = ad.getDistance();
    }
    
    //setters
    void setDistance(int d) {
        this.distance = d;
    }
    
    //getters
    int getDistance() {
        return this.distance;
    }
    
    //essentials
    public DistanceActivity clone() {
       return new DistanceActivity(this);
    }
    
    public String toString(){
        StringBuilder understring = new StringBuilder();
        understring.append("\nDistance: ");
        understring.append(this.distance);
        
        return super.toString() + understring.toString();
    }
    
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        DistanceActivity ad = (DistanceActivity) o;
       
       return ( super.equals(o) && this.distance == ad.getDistance() );
   }
}
*/
