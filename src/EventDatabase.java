/** EventDatabase class
 * Consists of a double entry table with memory shared through Event IDs
 * Events can be accessed by id or name
 * @author frmendes
 */

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

public class EventDatabase implements MappedDatabase<Event>, Serializable {
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
    public ArrayList<String> searchByName(String name) {
        ArrayList<String> result = new ArrayList<String>();
        for( String n: this.nameEntry.keySet())
            if( n.contains(name)) {
                result.add(n);
                System.out.println("YES\n");
            }

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
    public Event findById(int id) {
        try {
            return this.idEntry.get(id).clone();
        } catch (Exception e) {
            return null;
        }
    }

    /** Returns an event with the corresponding name
     * @param name
     * @return corresponding event
     */
    public Event findByName(String name) {
        try {
            return findById( this.nameEntry.get(name) );
        } catch(NullPointerException e) {
            return null;
        }
    }

    @Override
    public Set<Event> all() {
        HashSet<Event> copy = new HashSet<Event>();

        for ( Event e : this.idEntry.values() )
            copy.add( e.clone() );

        return (Set<Event>)copy;
    }

    public void removeUser(User u){
        for(Event e: this.idEntry.values()){
            try{
                e.removeParticipant(u);
            }
            catch( InexistingUserException ex){}
        }
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
    public ArrayList<Event> getEventList(){
        
        TreeSet<Event> set = toTreeSet(this.idEntry);
        
        ArrayList<Event> events = new ArrayList<Event>();
        for(Event e: set){
            events.add(e);
        }

        return events;
    }
    
    public ArrayList<Event> getUpcomingEvents() {
        TreeSet<Event> set = toTreeSet(this.idEntry);
        
        ArrayList<Event> list = new ArrayList<Event>();
        
        for(Event e : set ) {
            if( e.isUpcoming() )
                list.add( e.clone() );
        }
        
        return list;
    }
    
    public ArrayList<Event> getUpcomingEvents(String type) {
        TreeSet<Event> set = toTreeSet(this.idEntry);
        
        ArrayList<Event> list = new ArrayList<Event>();
        
        for(Event e : set ) {
            if( e.isUpcoming() && e.getType().equals(type) )
                list.add( e.clone() );
        }
        
        return list;
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
    public void delete(int id) {
        String name = findById(id).getName();
        this.idEntry.remove(id);
        this.nameEntry.remove(name);
    }

    @Override
    public void delete(Event e) {
        this.idEntry.remove( e.getId() );
        this.nameEntry.remove( e.getName() );
    }

   /**
     * Deletes the event with the corresponding name
     * @param name
     */
    public void delete(String name) {
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
    
    private TreeSet<Event> toTreeSet( HashMap<Integer, Event> map){
        TreeSet<Event> set = new TreeSet<Event>( new EventComparator() );
        
        for(Event e: map.values()){
            set.add(e);
        }
        
        return set;
    }
}