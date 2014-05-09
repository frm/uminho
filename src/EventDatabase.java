/**
 *
 * @author tiago
 */

import java.util.HashMap;

public class EventDatabase {
    private HashMap<Integer, Event> eventbase;
    private int eventCount;
    
    //constructors
    public EventDatabase(){
        this.eventbase = new HashMap<Integer, Event>();
        this.eventCount = 0;
    }

    public EventDatabase(EventDatabase ed){
        
        this.eventbase = ed.copyMap();
        this.eventCount = ed.nrEvents();
    }
    
    //methods
    public void addEvent(Event e){
        Event newEvent = e.clone();
        
        if(newEvent.getId() < 0)
            newEvent.setId(this.eventCount++);
        
        this.eventbase.put( newEvent.getId(), newEvent);
    }
    
    public void removeEvent(int id ){
        if(findEvent(id) != null)
            this.eventbase.remove(id);
    }
    
    public Event findEvent(int id){
        Event e = new Event();
        
        if(this.eventbase.containsKey(id)) e = this.eventbase.get(id).clone();
        else e = null;
        
        return e;
    }
    
    public int nrEvents(){return this.eventCount;}
    
    public HashMap<Integer, Event> copyMap(){
        HashMap<Integer, Event> copy = new HashMap<Integer, Event>();
        
        for(Event e: this.eventbase.values())
            copy.put(e.getId(), e.clone());
        
        return copy;
    }
    
    //essentials
    public boolean equals(Object o) {
        if(this == o) return true;

        if( o == null || this.getClass() != o.getClass() ) return false;

        EventDatabase eb = (EventDatabase) o;

       return this.eventbase.equals( eb.copyMap() );
    }
    
    public String toString() {
        StringBuilder sb = new StringBuilder();
        
        sb.append("TOTAL EVENTS: ");
        sb.append(this.nrEvents());
        sb.append("\n");
        sb.append(this.eventbase);
        
        return sb.toString();
    }
}