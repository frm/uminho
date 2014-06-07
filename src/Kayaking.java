/**
 *
 * @author tiago
 */
import java.util.GregorianCalendar;

/**
 *
 * @author joaorodrigues
 */
public class Kayaking extends DistanceActivity implements WeatherInterface{
    private Weather weather;
        
    //constructors

    /**
     *
     */
        public Kayaking() {
        super();
        this.weather = new Weather();
    }
    
    /**
     *
     * @param date
     * @param duration
     * @param distance
     * @param w
     */
    public Kayaking(GregorianCalendar date, long duration, int distance,  int w){
        super(date, duration, distance);
        this.weather = new Weather(w);
        this.setCalories( calculateCalories(duration, distance, w) );
    }
    
    /**
     *
     * @param model
     * @param w
     */
    public Kayaking(DistanceActivity model, int w){
        super(model);
        this.weather = new Weather(w);
        this.setCalories( calculateCalories(model.getDuration(), model.getDistance(), w) );
    }
    
    /**
     *
     * @param k
     */
    public Kayaking(Kayaking k){
        super(k);
        this.weather = k.getWeather();
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

    /**
     *
     * @param duration
     * @param distance
     * @param weather
     * @return
     */
        public int calculateCalories(long duration, int distance, int weather){
        double wfac = Weather.calculateWeatherFactor(weather);
        int randomVariance = (int) (Math.random()*20);
        return ((int) (((double)distance/(double)duration)*650000.0*wfac)) + randomVariance;
    }

        private double calculateWeatherFactor(int w){
        if (w < 2) return 1;
        else if (w < 5) return 1.05;
        else if (w == 6) return 1.1;
        else return 1.2;       
    }
}
