/**
 *
 * @author tiago
 */

import java.util.HashSet;

public class EventList {
    private HashSet<Integer> events;
    
    //constructors
    public EventList(){this.events = new HashSet<Integer>();}
    
    public EventList(HashSet<Integer> events){
        this.events = new HashSet<Integer>();
        for(Integer i: events)
            (this.events).add(i);
    }
    
    public EventList(EventList e){
        this.events = new HashSet<Integer>();
        for( Integer i: e.getEvents() )
            (this.events).add(i);
    }
    
    //getters
    public void setEvents(HashSet<Integer> events){
        this.events = events;
    }
    
    //setters
    public HashSet<Integer> getEvents(){
        return (HashSet<Integer>)events.clone();
    }
    
    //methods
    public void addEvent(Integer id) {this.events.add(id);}
    public void removeEvent(Integer id) {this.events.remove(id);}
    public Integer numberofEvents() {return this.events.size();}
    
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
        EventList events2 = new EventList();
        try {
            new EventList(this);
        } catch (IllegalArgumentException e) {
            System.err.println("Unexisting events2 list");
            throw new IllegalArgumentException( e.getMessage() );
        }
        
        return events2;
    }
}