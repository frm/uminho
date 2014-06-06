/**
 *
 * @author tiago
 */
import java.util.GregorianCalendar;

public class Kayaking extends DistanceActivity implements WeatherInterface{
    private Weather weather;
        
    //constructors
    public Kayaking() {
        super();
        this.weather = new Weather();
    }
    
    public Kayaking(GregorianCalendar date, long duration, int distance,  int w){
        super(date, duration, distance);
        this.weather = new Weather(w);
        this.setCalories( calculateCalories(duration, distance, w) );
    }
    
    public Kayaking(DistanceActivity model, int w){
        super(model);
        this.weather = new Weather(w);
        this.setCalories( calculateCalories(model.getDuration(), model.getDistance(), w) );
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
    {return new Kayaking(this);}
    
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
    public int calculateCalories(long duration, int distance, int weather){
        double wfac = Weather.calculateWeatherFactor(weather);
        return (int) (((double)distance/(double)duration)*650000.0*wfac);
    }

    
}
