/**
 *
 * @author tiago
 */
import java.util.GregorianCalendar;

public class Cycling extends AltitudeActivity implements weatherInterface{
    private Weather weather;
    
    //constructors
    public Cycling() {
        super();
        this.weather = new Weather();
    }

    public Cycling(GregorianCalendar date, GregorianCalendar duration, int distance, int altitude, int w){
        super(date, duration, distance, altitude);
        this.weather = new Weather(w);
        this.setCalories( calculateCalories(duration, distance, w) );
    }
    
    public Cycling(Cycling c){
        super(c);
        this.weather = c.getWeather();
    }

    public Weather getWeather() {
        return weather;
    }

    public void setWeather(Weather weather) {
        this.weather = weather;
    }

    
    
    //essentials
    public Cycling clone()
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
       
        Cycling c = (Cycling) o;
       
        return ( super.equals(o) && this.weather.equals( c.getWeather() ) );
    }
    
    //methods
    public double calculateCalories(GregorianCalendar duration, int distance, int weather){
        double wfac = calculateWeatherFactor(weather);
        return ((double)((altitude + distance)/duration.getTimeInMillis()))*950000*wfac;
    }
    
    double calculateWeatherFactor(int w){
        if (w < 2) return 1;
        else if (w < 5) return 1.05;
        else if (w == 6) return 1.1;
        else return 1.2;       
    }
}

