/**
 *
 * @author frmendes
 */

import java.util.Arrays;

public class Weather {
    private int weatherIndex;
    
    private static final String[] weatherStates = {"Indoor", "Clear Sky", "Cloudy", "Light Rain", "Heavy Rain", "Snowing", "Hailing", "Windy", "Stormy"};
    
    public Weather() {
        this.weatherIndex = 0;
    }
    
    public Weather(int index) {
        this.weatherIndex = index;
    }
    
    public Weather(Weather w) {
        this.weatherIndex = w.getWeatherIndex();
    }
    
    public Weather(String weather) {
        this.weatherIndex = Weather.getIndexOf(weather);
    }
    
    public String getWeatherSTR() {
        return Weather.weatherStates[this.weatherIndex];
    }
    
    public int getWeatherIND() {
        return Weather.this.weatherIndex;
    }
    
    public void setWeather(int index) {
        this.weatherIndex = index;
    }
    
    public void setWeather(String weather) {
        this.weatherIndex = Weather.getIndexOf(weather);
    }

    @Override
    public Weather clone() {
        return new Weather(this);
    }

    @Override
    public String toString() {
        return this.getWeatherSTR();
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
    
    private static int getIndexOf(String weather) {
        return Arrays.asList(Weather.weatherStates).indexOf(weather);
    }
}
