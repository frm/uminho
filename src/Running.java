/**
 *
 * @author tiago
 */
import java.util.GregorianCalendar;

public class Running extends AltitudeActivity implements WeatherInterface{
    private Weather weather;
    
    //constructors
    public Running() {
        super();
        this.weather = new Weather();
    }
    
    public Running(GregorianCalendar date, long duration, int distance, int altitude, int w){
        super(date, duration, distance, altitude);
        this.weather = new Weather(w);
        this.setCalories( calculateCalories(duration, distance, altitude, w) );
    }
    
    public Running(AltitudeActivity model, int w){
        super(model);
        this.weather = new Weather(w);
        this.setCalories( calculateCalories(model.getDuration(), model.getDistance(), model.getAltitude(), w) );
    }
    
    public Running(Running r){
        super(r);
        this.weather = r.getWeather();
    }
    
    public Weather getWeather() {
        return weather;
    }

    public void setWeather(Weather weather) {
        this.weather = weather;
    }
    
    //essentials
    public Running clone()
    {return new Running(this);}
    
    public String toString(){
        StringBuilder understring = new StringBuilder();
        understring.append("\nWeather: ");
        understring.append(this.weather);
        
        return super.toString() + understring.toString();
    }
    
    public boolean equals(Object o) {
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Running r = (Running) o;
       
        return ( super.equals(o) && this.weather.equals( r.getWeather() ) );
    }
    
    
    //methods
    public int calculateCalories(long duration, int distance, int altitude, int weather){
        double wfac = Weather.calculateWeatherFactor(weather);
        return (int) ( (  ( (double) (altitude + distance)/ (double) duration) )*950000.0*wfac);
    }
    
   
}
