/**
 *
 * @author frmendes
 */

import java.io.Serializable;
import java.util.Arrays;

/**
 *
 * @author joaorodrigues
 */
public class Weather implements Serializable{
    private int weatherIndex;
    
    /**
     *
     */
    public static final String[] weatherStates = { "Clear Sky", "Cloudy", "Light Rain", "Heavy Rain", "Snowing", "Hailing", "Windy", "Stormy"};
    
    /**
     *
     */
    public Weather() {
        this.weatherIndex = 0;
    }
    
    /**
     *
     * @param index
     */
    public Weather(int index) {
        this.weatherIndex = index;
    }
    
    /**
     *
     * @param w
     */
    public Weather(Weather w) {
        this.weatherIndex = w.getWeatherIndex();
    }
    
    /**
     *
     * @param weather
     */
    public Weather(String weather) {
        this.weatherIndex = Weather.getIndexOf(weather);
    }
    
    /**
     *
     * @return
     */
    public String getWeather() {
        return Weather.weatherStates[this.weatherIndex];
    }
    
    /**
     *
     * @param index
     */
    public void setWeather(int index) {
        this.weatherIndex = index;
    }
    
    /**
     *
     * @param weather
     */
    public void setWeather(String weather) {
        this.weatherIndex = Weather.getIndexOf(weather);
    }

    @Override
    public Weather clone() {
        return new Weather(this);
    }

    @Override
    public String toString() {
        return this.getWeather();
    }
    
    public boolean equals(Object o) {
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Weather w = (Weather) o;
       
       return this.weatherIndex == w.getWeatherIndex();
    }
    
    private int getWeatherIndex() {
        return this.weatherIndex;
    }
    
    /**
     *
     * @param weather
     * @return
     */
    public static int getIndexOf(String weather) {
        return Math.max( Arrays.asList(Weather.weatherStates).indexOf(weather), 0);
    }
    
    /**
     *
     * @param w
     * @return
     */
    public static double calculateWeatherFactor(int w){
        if (w < 2) return 1;
        else if (w < 5) return 1.05;
        else if (w == 6) return 1.1;
        else return 1.2;       
    }
}
