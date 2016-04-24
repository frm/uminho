/**
 *
 * @author tiago
 */
import java.util.GregorianCalendar;

/**
 *
 * @author joaorodrigues
 */
public class Running extends AltitudeActivity implements WeatherInterface{
    private Weather weather;
    
    //constructors

    /**
     *
     */
        public Running() {
        super();
        this.weather = new Weather();
    }
    
    /**
     *
     * @param date
     * @param duration
     * @param distance
     * @param altitude
     * @param w
     */
    public Running(GregorianCalendar date, long duration, int distance, int altitude, int w){
        super(date, duration, distance, altitude);
        this.weather = new Weather(w);
        this.setCalories( calculateCalories(duration, distance, altitude, w) );
    }
    
    /**
     *
     * @param model
     * @param w
     */
    public Running(AltitudeActivity model, int w){
        super(model);
        this.weather = new Weather(w);
        this.setCalories( calculateCalories(model.getDuration(), model.getDistance(), model.getAltitude(), w) );
    }
    
    /**
     *
     * @param r
     */
    public Running(Running r){
        super(r);
        this.weather = r.getWeather();
    }
    
    /**
     *
     * @return
     */
    public Weather getWeather() {
        return weather;
    }

    /**
     *
     * @param weather
     */
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

    /**
     *
     * @param duration
     * @param distance
     * @param altitude
     * @param weather
     * @return
     */
        public int calculateCalories(long duration, int distance, int altitude, int weather){
        double wfac = Weather.calculateWeatherFactor(weather);
        int randomVariance = (int) (Math.random()*20);
        return ((int) ( (  ( (double) (altitude + distance)/ (double) duration) )*250000.0*wfac)) + randomVariance;
    }
    
    private double calculateWeatherFactor(int w){
        if (w < 2) return 1;
        else if (w < 5) return 1.05;
        else if (w == 6) return 1.1;
        else return 1.2;       
    }
   
}
