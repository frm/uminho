/**
 *
 * @author tiago
 */
import java.util.GregorianCalendar;

public class Kayaking extends DistanceActivity implements weatherInterface{
    private Weather weather;
        
    //constructors
    public Kayaking() {
        super();
        this.weather = new Weather();
    }
    
    public Kayaking(GregorianCalendar date, GregorianCalendar duration, int distance, int w){
        super(date, duration, distance);
        this.weather = new Weather(w);
        this.setCalories( calculateCalories(duration, distance, w) );
    }
    
    public Kayaking(Kayaking k){
        super(k);
        this.weather = k.getWeather();
    }
    
    public Weather getWeather() {
        return weather;
    }

    public void setWeather(Weather weather) {
        this.weather = weather;
    }
    
    //essentials
    public Kayaking clone()
    {return this.clone();}
    
    public String toString(){
        StringBuilder understring = new StringBuilder();
        understring.append("\nWeather: ");
        understring.append(this.weather);
        
        return super.toString() + understring.toString();
    }
    
    public boolean equals(Object o) {
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Kayaking k = (Kayaking) o;
       
        return ( super.equals(o) && this.weather.equals( k.getWeather() ) );
    }
    
    //methods
    public double calculateCalories(GregorianCalendar duration, int distance, int weather){
        double wfac = calculateWeatherFactor(weather);
        return (double)(distance/duration.getTimeInMillis())*650000*wfac;
    }

    double calculateWeatherFactor(int w){
        if (w < 2) return 1;
        else if (w < 5) return 1.05;
        else if (w == 6) return 1.1;
        else return 1.2;       
    }
}
