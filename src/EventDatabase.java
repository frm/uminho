/** EventDatabase class
 * Consists of a double entry table with memory shared through Event IDs
 * Events can be accessed by id or name
 * @author frmendes
 */

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

public class EventDatabase implements MappedDatabase<Event> {
    private HashMap<Integer, Event> idEntry;
    private HashMap<String, Integer> nameEntry;
    private int eventCount;
    
    public EventDatabase() {
        this.idEntry = new HashMap<Integer, Event>();
        this.nameEntry = new HashMap<String, Integer>();
        this.eventCount = 0;
    }

    // This is wrong. Change this
    public EventDatabase(EventDatabase ed) {
        this.idEntry = ed.getIdEntry();
        this.eventCount = ed.nrEvents();
    }
    
    public int nrEvents() {
        return this.eventCount;
    }
    
    @Override
    public void save(Event e) {
        Event newEvent = e.clone();
        
        if(newEvent.getId() < 0)
            newEvent.setId(++this.eventCount);
        
        this.idEntry.put( newEvent.getId(), newEvent );
    }
    
    public Event findById(int id) throws InexistingEventException {
        return this.idEntry.get(id).clone();
    }
    
    @Override
    public Set<Event> all() {
        HashSet<Event> copy = new HashSet<Event>();

        for ( Event e : this.idEntry.values() )
            copy.add( e.clone() );

        return (Set<Event>)copy;
    }
    
    @Override
    public HashMap<Integer, Event> getIdEntry(){
        HashMap<Integer, Event> copy = new HashMap<Integer, Event>();
        
        for( Event e: this.idEntry.values() )
            copy.put(e.getId(), e.clone());
        
        return copy;
    }
    
    @Override
    public boolean equals(Object o) {
        if(this == o) return true;

        if( o == null || this.getClass() != o.getClass() ) return false;

        EventDatabase eb = (EventDatabase) o;

       return this.idEntry.equals( eb.copyMap() );
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        
        sb.append("TOTAL EVENTS: ");
        sb.append(this.nrEvents());
        sb.append("\n");
        sb.append(this.idEntry);
        
        return sb.toString();
    }
    
    private HashMap<String, Integer> getNameEntry() {
        HashMap<String, Integer> cpy = new HashMap<String, Integer>();
        cpy.putAll(this.nameEntry);
        return cpy;
    }
    
    private HashMap<String, Integer> getTypeEntry() {
        HashMap<String, Integer> cpy = new HashMap<String, Integer>();
        cpy.putAll(this.typeEntry);
        return cpy;
    }
}