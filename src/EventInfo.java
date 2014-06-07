
import java.util.GregorianCalendar;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author frmendes
 */
public class EventInfo {
    private int capacity;
    private String name;
    private String location;
    private Weather weather;
    private GregorianCalendar date;
    private GregorianCalendar signupLimit;
    
    /** Default number of participants, given in case the number is not valid or given */
    private static final int defaultParticipants = Integer.MAX_VALUE;
    
    public EventInfo() {
        setCapacity(0);
        this.name = "";
        this.location = "";
        this.weather = new Weather();
        this.date = new GregorianCalendar();
        this.signupLimit = new GregorianCalendar();
    }
    
    public EventInfo(EventInfo info) {
        setCapacity( info.getCapacity() );
        this.name = info.getName();
        this.location = info.getLocation();
        this.weather = new Weather( info.getWeather() );
        this.date = (GregorianCalendar)info.getDate();
        this.signupLimit = (GregorianCalendar)info.getSignupLim();
    }
    
    public EventInfo(int capacity, String name, String location, int weather, GregorianCalendar date, GregorianCalendar signup) {
        setCapacity(capacity);
        this.name = name;
        this.location = location;
        this.weather = new Weather(weather);
        this.date = (GregorianCalendar)date.clone();
        this.signupLimit = (GregorianCalendar)signup.clone();
    }
    

    /** Sets the event name
     * @param s name of the event
      */
    public void setName(String s) {
        this.name = s;
    }
    
    public void setLocation(String s) {
        this.location = s;
    }
    
    public void setSignupLimit(GregorianCalendar date) {
        this.signupLimit = (GregorianCalendar)date.clone();
    }

    /** Sets the expected weather for the event
     * @param w expected weather
     */
    public void setWeather(int w) {
        this.weather.setWeather(w);
    }
    
    /** Sets the date for the event
     * @param date Date of the event
     */
    public void setDate(GregorianCalendar date) {
        this.date = (GregorianCalendar)date.clone();
    }

    /** Returns the name of the event
     * @return name of the event
     */
    public String getName() {
        return this.name;
    }
    
    public String getLocation() {
        return this.location;
    }
    
    public GregorianCalendar getSignupLim() {
        return (GregorianCalendar)this.signupLimit.clone();
    }

    /** Returns the maximum number of users allowed
     * @return capacity of the event
     */
    public int getCapacity() {
        return this.capacity;
    }

    /** Returns the expected weather for the event
     * @return event weather
     */
    public String getWeather() {
        return this.weather.getWeather();
    }

    /** Returns the date of the event
     * @return date of the event
     */
    public GregorianCalendar getDate() {
        return (GregorianCalendar)this.date.clone();
    }
    
    /** Sets the event capacity
     * It shall fallback to Event.defaultParticipants in case the given number is < 0
     * @param c capacity
     */
    public void setCapacity(int c) {
        if (c > 0)
            this.capacity = c;
        else
            this.capacity = EventInfo.defaultParticipants;
    }
    
    public boolean isUpcoming() {
        return this.date.compareTo( new GregorianCalendar() ) > 0;
    }

    @Override
    public EventInfo clone() {
        return new EventInfo(this);
    }

    @Override
    public boolean equals(Object o) {
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass()) return false;

        EventInfo e = (EventInfo) o;
        
        return (
                this.name.equals( e.getName() ) &&
                this.location.equals( e.getLocation() ) &&
                this.date.equals( e.getDate() ) &&
                this.signupLimit.equals( e.getSignupLim() ) &&
                this.capacity == e.getCapacity() &&
                this.getWeather().equals( e.getWeather() )
                );
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Event Info: \n");
        sb.append("\nName: ");
        sb.append(this.name);
        sb.append("\nLocation: ");
        sb.append(this.location);
        sb.append("\nDate");
        sb.append(this.date);
        sb.append("\nWeather Prediction: ");
        sb.append(this.weather);
        sb.append("\nMax entries: ");
        sb.append(this.capacity);
        sb.append("\nSignup Limit: ");
        sb.append(this.signupLimit);
        
        return sb.toString();
    }
}
