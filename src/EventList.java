/**
 *
 * @author tiago
 */

import java.io.Serializable;
import java.util.HashSet;

/**
 *
 * @author joaorodrigues
 */
public class EventList implements Serializable{
    private HashSet<Integer> events;
    
    //constructors

    /**
     *
     */
        public EventList(){
        this.events = new HashSet<Integer>();
    }
    
    /**
     *
     * @param events
     */
    public EventList(HashSet<Integer> events) {
        this.events = EventList.copyEvents(events);
    }
    
    /**
     *
     * @param e
     */
    public EventList(EventList e){
        this.events = e.getEvents();
    }
    
    //getters

    /**
     *
     * @param events
     */
        public void setEvents(HashSet<Integer> events){
        this.events = EventList.copyEvents(events);
    }
    
    //setters

    /**
     *
     * @return
     */
        public HashSet<Integer> getEvents(){
        return EventList.copyEvents(this.events);
    }
    
    //methods

    /**
     *
     * @param id
     */
        public void addEvent(int id) {
        this.events.add(id);
    }
    
    /**
     *
     * @param id
     */
    public void removeEvent(int id) {
        this.events.remove(id);
    }
    
    /**
     *
     * @param id
     * @return
     */
    public boolean participatedIn(int id) {
        return this.events.contains(id);
    }
    
    /**
     *
     * @return
     */
    public int numberOfEvents() {
        return this.events.size();
    }
    
    /**
     *
     * @param e
     * @return
     */
    public boolean isInEvent(Event e){
        return this.events.contains(e.getId());
    }
    
    //essentials
    public String toString(){
        StringBuilder sb = new StringBuilder();
    
        for(Integer i : this.events){
            sb.append(i.toString());
            sb.append(" ");
        }
        
        return sb.toString();
    }
    
    public EventList clone(){
        return new EventList(this);
    }
    
    private static HashSet<Integer> copyEvents(HashSet<Integer> e) {
        HashSet<Integer> list = new HashSet<Integer>();
        for(int i : e)
            list.add(i);
        
        return list;
    }
    
    
}