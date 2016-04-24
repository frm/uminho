/**
 * Class containing a set of Event ids
 * @author tiago
 */

import java.io.Serializable;
import java.util.HashSet;


public class EventList implements Serializable{
    private HashSet<Integer> events;
    
    /**
     * Empty constructor
     */
        public EventList(){
        this.events = new HashSet<Integer>();
    }
    
    /**
     * Parameterized constructor
     * @param events
     */
    public EventList(HashSet<Integer> events) {
        this.events = EventList.copyEvents(events);
    }
    
    /**
     * Copy constructor
     * @param e
     */
    public EventList(EventList e){
        this.events = e.getEvents();
    }
    

    /**
     * Sets the events
     * @param events
     */
        public void setEvents(HashSet<Integer> events){
        this.events = EventList.copyEvents(events);
    }
    

    /**
     * Returns a copy of the current set of events
     * @return
     */
        public HashSet<Integer> getEvents(){
        return EventList.copyEvents(this.events);
    }
    

    /**
     * Adds an event
     * @param id
     */
        public void addEvent(int id) {
        this.events.add(id);
    }
    
    /**
     * Removes an event
     * @param id
     */
    public void removeEvent(int id) {
        this.events.remove(id);
    }
    
    /**
     * Determines if a user has participated in an event
     * @param id id of the user
     * @return
     */
    public boolean participatedIn(int id) {
        return this.events.contains(id);
    }
    
    /**
     * Returns the number of events
     * @return
     */
    public int numberOfEvents() {
        return this.events.size();
    }
    
    /**
     * Determines if an event exists in the current set
     * @param e
     * @return
     */
    public boolean isInEvent(Event e){
        return this.events.contains(e.getId());
    }
    

    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder();
    
        for(Integer i : this.events){
            sb.append(i.toString());
            sb.append(" ");
        }
        
        return sb.toString();
    }
    
    @Override
    public EventList clone(){
        return new EventList(this);
    }
    
    /**
     * Makes a copy of the given set of events
     * @param e
     * @return 
     */
    private static HashSet<Integer> copyEvents(HashSet<Integer> e) {
        HashSet<Integer> list = new HashSet<Integer>();
        for(int i : e)
            list.add(i);
        
        return list;
    }
    
    
}