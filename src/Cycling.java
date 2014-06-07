/**
 *
 * @author tiago
 */
import java.util.GregorianCalendar;

public class Cycling extends AltitudeActivity implements WeatherInterface{
    private Weather weather;
    
    //constructors
    public Cycling() {
        super();
        this.weather = new Weather();
    }

    public Cycling(GregorianCalendar date, long duration, int distance, int altitude, int w){
        super(date, duration, distance, altitude);
        this.weather = new Weather(w);
        this.setCalories( calculateCalories(duration, distance, altitude,  w) );
    }
    
    public Cycling(GregorianCalendar date, long duration, int distance, int altitude){
        super(date, duration, distance, altitude);
    }
    
    public Cycling(AltitudeActivity model, int weather){
        super(model);
        this.weather = new Weather(weather);
        this.setCalories( calculateCalories(model.getDuration(), model.getDistance(), model.getAltitude(),  weather) );
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
    {return new Cycling(this);}
    
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
    private int calculateCalories(long duration, int distance, int altitude,  int weather){
        double wfac = calculateWeatherFactor(weather);
        int randomVariance = (int) (Math.random()*20);
        return ((int) ( ( (double)(altitude + distance)/(double)duration ) * 95000.0 * wfac )) + randomVariance;
    }
    
    private double calculateWeatherFactor(int w){
        if (w < 2) return 1;
        else if (w < 5) return 1.05;
        else if (w == 6) return 1.1;
        else return 1.2;       
    }
}

