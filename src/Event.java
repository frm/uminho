/**
 *
 * @author tiago
 */
import java.util.HashSet;

/**
 *
 * @author joaorodrigues
 */
public class Event {
    private int id;
    private String name;
    private int capacity;
    private Weather weather;
    private UserList entries;
    
    //constructors

    /**
     *
     */
        public Event(){
        this.id = -1;
        this.name = "testEvent";
        this.capacity = 10;
        this.weather = new Weather();
        this.entries = new UserList();  
    }
    
    /**
     *
     * @param id
     * @param name
     * @param capacity
     * @param weatherstr
     * @param entries
     */
    public Event(int id, String name, int capacity, String weatherstr, UserList entries){
        this.id = id;
        this.name = name;
        this.capacity = capacity;
        this.weather = new Weather(weatherstr);
        this.entries = entries.clone();
    }
    
    /**
     *
     * @param e
     */
    public Event(Event e){
        this.id = e.getId();
        this.name = e.getName();
        this.capacity = e.getCapacity();
        this.weather = new Weather (e.getWeather());
        this.entries = e.getEntries();
    }
    //setters

    /**
     *
     * @param id
     */
        public void setId(int id) {this.id = id;}

    /**
     *
     * @param s
     */
    public void setName(String s) {this.name = s;}

    /**
     *
     * @param c
     */
    public void setCapacity(int c) {this.capacity = c;}

    /**
     *
     * @param w
     */
    public void setWeather(int w) {this.weather.setWeather(w);}

    /**
     *
     * @param e
     */
    public void setEntries(UserList e) {this.entries = e.clone();}
    
    //getters

    /**
     *
     * @return
     */
        public int getId() {return this.id;}

    /**
     *
     * @return
     */
    public String getName() {return this.name;}

    /**
     *
     * @return
     */
    public int getCapacity() {return this.capacity;}

    /**
     *
     * @return
     */
    public String getWeather() {return this.weather.getWeather();}

    /**
     *
     * @return
     */
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
        
        return(this.id == e.getId() && this.name.equals(e.getName()) && this.weather.getWeather().equals(e.getWeather()) && this.capacity == e.getCapacity() && this.entries.equals(e.getEntries()));
    }
}
