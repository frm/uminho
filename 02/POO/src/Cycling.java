
import java.util.GregorianCalendar;

/**Cycling Activity
 *
 * @author joaorodrigues
 */
public class Cycling extends AltitudeActivity implements WeatherInterface{
    private Weather weather;
    
    //constructors

    /**
     *
     */
        public Cycling() {
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
    public Cycling(GregorianCalendar date, long duration, int distance, int altitude, int w){
        super(date, duration, distance, altitude);
        this.weather = new Weather(w);
        this.setCalories( calculateCalories(duration, distance, altitude,  w) );
    }
    
    /**Constructor with only the basic information
     *
     * @param date
     * @param duration
     * @param distance
     * @param altitude
     */
    public Cycling(GregorianCalendar date, long duration, int distance, int altitude){
        super(date, duration, distance, altitude);
    }
    
    /**Constructor that uses a model AltitudeActivity for part of the information, and info for the weather
     *
     * @param model
     * @param weather
     */
    public Cycling(AltitudeActivity model, int weather){
        super(model);
        this.weather = new Weather(weather);
        this.setCalories( calculateCalories(model.getDuration(), model.getDistance(), model.getAltitude(),  weather) );
    }
    
    /**
     *
     * @param c
     */
    public Cycling(Cycling c){
        super(c);
        this.weather = c.getWeather();
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
    @Override
    public Cycling clone()
    {return new Cycling(this);}
    
    @Override
    public String toString(){
        StringBuilder understring = new StringBuilder();
        understring.append("\nWeather: ");
        understring.append(this.weather);
        
        return super.toString() + understring.toString();
    }
    
    @Override
    public boolean equals(Object o) {
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Cycling c = (Cycling) o;
       
        return ( super.equals(o) && this.weather.equals( c.getWeather() ) );
    }
    
    /**Calorie calculation for this activity
     * 
     * @param duration
     * @param distance
     * @param altitude
     * @param weather
     * @return 
     */
    private int calculateCalories(long duration, int distance, int altitude,  int weather){
        double wfac = calculateWeatherFactor(weather);
        int randomVariance = (int) (Math.random()*20);
        return ((int) ( ( (double)(altitude + distance)/(double)duration ) * 95000.0 * wfac )) + randomVariance;
    }
    
    /**Weather factor calculation
     * 
     * @param w
     * @return 
     */
    private double calculateWeatherFactor(int w){
        if (w < 2) return 1;
        else if (w < 5) return 1.05;
        else if (w == 6) return 1.1;
        else return 1.2;       
    }
}

