import java.util.GregorianCalendar;

public class ActivityAltitude extends ActivityDistance{
    int altitude;
    
    //constructors
    public ActivityAltitude(){
       super();
       this.altitude = 0;
   }
   
    public ActivityAltitude(String n, String w, GregorianCalendar date, GregorianCalendar duration, int c, int distance, int altitude){
        super();
        this.altitude = altitude;
    }
   
    public ActivityAltitude(ActivityAltitude aa){
       super();
       this.altitude = aa.getAltitude();
   }
   
    //setters
    void setAltitude(int a){this.altitude = a;}
    
    //getters
    int getAltitude(){return this.altitude;}
    
    //essentials
    public ActivityAltitude clone(){
       return new ActivityAltitude(this);
    }
    
    public String toString(){
        StringBuilder understring = new StringBuilder();
        understring.append("\nAltitude");
        understring.append(this.altitude);
        
        return super.toString() + understring.toString();
    }
    
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        ActivityAltitude aa = (ActivityAltitude) o;
       
       return (super.equals(o) && this.altitude == aa.getAltitude());
    }
}