/**
 *
 * @author tiago
 */

import java.util.HashSet;

public class EventList {
    private HashSet<Integer> events;
    
    //constructors
    public EventList(){
        this.events = new HashSet<Integer>();
    }
    
    public EventList(HashSet<Integer> events) {
        this.events = EventList.copyEvents(events);
    }
    
    public EventList(EventList e){
        this.events = e.getEvents();
    }
    
    //getters
    public void setEvents(HashSet<Integer> events){
        this.events = EventList.copyEvents(events);
    }
    
    //setters
    public HashSet<Integer> getEvents(){
        return EventList.copyEvents(this.events);
    }
    
    //methods
    public void addEvent(int id) {
        this.events.add(id);
    }
    
    public void removeEvent(int id) {
        this.events.remove(id);
    }
    
    public boolean participatedIn(int id) {
        return this.events.contains(id);
    }
    
    public int numberOfEvents() {
        return this.events.size();
    }
    
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