/** EventDatabase class
 * Consists of a double entry table with memory shared through Event IDs
 * Events can be accessed by id or name
 * @author frmendes
 */

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
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

    public EventDatabase(EventDatabase ed) {
        this.idEntry = new HashMap<Integer, Event>();
        this.nameEntry = new HashMap<String, Integer>();
        this.eventCount = ed.nrEvents();
        this.copyEvents(ed);
    }
    
    /**
     * Searches for an event, by name
     * @param name
     * @return  ArrayList with the search results
     */
    public ArrayList<String> searchByName(String name) throws NoEventsAvailableException{
        ArrayList<String> result = new ArrayList<String>();
        for( String n: this.nameEntry.keySet())
            if( n.contains(name)) result.add(n);
        if(result == null)
            throw new NoEventsAvailableException();
        
        return result;
    }
    
    /**
     * Returns the total number of events in the database
     * @return 
     */
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
    
    @Override
    public Event findById(int id) throws InexistingEventException {
        return this.idEntry.get(id).clone();
    }
    
    /** Returns an event with the corresponding name
     * @param name
     * @return corresponding event
     * @throws InexistingEventException 
     */
    public Event findByName(String name) throws InexistingEventException {
        return findById( this.nameEntry.get(name) );
    }
    
    @Override
    public Set<Event> all() {
        HashSet<Event> copy = new HashSet<Event>();

        for ( Event e : this.idEntry.values() )
            copy.add( e.clone() );

        return (Set<Event>)copy;
    }
    
    /**
     * Returns the current database entries mapped by name and id
     * @return database name entries
     */
    private HashMap<String, Integer> getNameEntry() {
        HashMap<String, Integer> cpy = new HashMap<String, Integer>();
        cpy.putAll(this.nameEntry);
        return cpy;
    }
    
    @Override
    public Map<Integer, Event> getIdEntry() {
        HashMap<Integer, Event> copy = new HashMap<Integer, Event>();
        
        for( Event e : this.idEntry.values() )
            copy.put(e.getId(), e.clone());
        
        return copy;
    }
    
    /**
     * Gets the events as an ArrayList, to navigate
     * @return ArrayList of events
     */
    public ArrayList<Event> getEventList() throws NoEventsAvailableException{
        ArrayList<Event> events = new ArrayList<Event>();
        for(Event e: this.idEntry.values()){
            events.add(e);
        }
        
        if(events == null)
            throw new NoEventsAvailableException();
        return events;
    }
    
    @Override
    public boolean equals(Object o) {
        if(this == o) return true;

        if( o == null || this.getClass() != o.getClass() ) return false;

        EventDatabase ed = (EventDatabase) o;

       return (
               this.idEntry.equals( (HashMap<Integer, Event>)ed.getIdEntry() ) &&
               this.nameEntry.equals( ed.getNameEntry() ) &&
               this.eventCount == ed.nrEvents()
               );
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
    
    @Override
    public void delete(int id) throws InexistingEventException {
        String name = findById(id).getName();
        this.idEntry.remove(id);
        this.nameEntry.remove(name);
    }

    @Override
    public void delete(Event e) throws InexistingEventException {
        this.idEntry.remove( e.getId() );
        this.nameEntry.remove( e.getName() );
    }
    
   /**
     * Deletes the event with the corresponding name
     * @param name
     * @throws InexistingEventException 
     */
    public void delete(String name) throws InexistingEventException {
        int id = this.nameEntry.get(name);
        this.idEntry.remove(id);
        this.nameEntry.remove(name);
    }
    
    @Override
    public EventDatabase clone() {
        return new EventDatabase(this);
    }
    
    /**
     * Copies the events from one database to another
     * @param ed 
     */
    private void copyEvents(EventDatabase ed) {
        for( Event e : ed.all() ) {
            int id = e.getId();
            this.idEntry.put(id, e);
            this.nameEntry.put(e.getName(), id);
        }            
    }
}