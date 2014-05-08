import java.util.GregorianCalendar; 

public class ActivityDistance extends Activity{
    int distance;
    
    //constructors
    public ActivityDistance(){
        super();
        this.distance = 0;
    }
    
    public ActivityDistance(String n, String w, GregorianCalendar date, GregorianCalendar duration, int c, int distance){
        super();
        this.distance = distance;
    }
    
    public ActivityDistance(ActivityDistance ad){
        super();
        this.distance = ad.getDistance();
    }
    
    //setters
    void setDistance(int d){this.distance = d;}
    
    //getters
    int getDistance(){return this.distance;}
    
    //essentials
    public ActivityDistance clone(){
       return new ActivityDistance(this);
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
       
        ActivityDistance ad = (ActivityDistance) o;
       
       return (super.equals(o) && this.distance == ad.getDistance());
   }
}
