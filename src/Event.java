/**
 *
 * @author tiago
 */
import java.util.HashSet;

public class Event {
    private int id;
    private String name;
    private int capacity;
    private Weather weather;
    private UserList entries;
    
    //constructors
    public Event(){
        this.id = -1;
        this.name = "testEvent";
        this.capacity = 10;
        this.weather = new Weather();
        this.entries = new UserList();  
    }
    
    public Event(int id, String name, int capacity, int weatherint, UserList entries){
        this.id = id;
        this.name = name;
        this.capacity = capacity;
        this.weather = new Weather(weatherint);
        this.entries = entries.clone();
    }
    
    public Event(Event e){
        this.id = e.getId();
        this.name = e.getName();
        this.capacity = e.getCapacity();
        this.weather = new Weather (e.getWeather());
        this.entries = e.getEntries();
    }
    //setters
    public void setId(int id) {this.id = id;}
    public void setName(String s) {this.name = s;}
    public void setCapacity(int c) {this.capacity = c;}
    public void setWeather(int w) {this.weather.setWeather(w);}
    public void setEntries(UserList e) {this.entries = e.clone();}
    
    //getters
    public int getId() {return this.id;}
    public String getName() {return this.name;}
    public int getCapacity() {return this.capacity;}
    public int getWeather() {return this.weather.getWeatherIND();}
    public UserList getEntries() {return this.entries.clone();}
    
    //methods add, remove, addUser, removeUser
    
    //essentials
    public Event clone() {
        return new Event(this);
    }
    
    
    
    public String toString(){
        StringBuilder sb = new StringBuilder();
        
        sb.append("### Event ###\n");
        sb.append("Id: ");
        sb.append(this.id);
        sb.append("\nName: ");
        sb.append(this.name);
        sb.append("\nWeather Prediction: ");
        sb.append(this.weather);
        sb.append("\nMax entries: ");
        sb.append(this.capacity);
        sb.append("\nParticipants: ");
        sb.append(this.entries);
        
        return sb.toString();
    }
    
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass()) return false;
        
        Event e = (Event) o;
        
        return(this.id == e.getId() && this.name.equals(e.getName()) && this.weather.getWeatherIND() == e.getWeather() && this.capacity == e.getCapacity() && this.entries.equals(e.getEntries()));
    }
}
