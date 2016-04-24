
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;

/**
 * Class that contains a database of events and has the necessary methods to manage their interaction<br>
 * As well as the user interaction
 */

/**
 *
 * @author frmendes
 */
public class EventController implements Serializable{
    private EventDatabase database;

    /**
     * Empty constructor
     */
    public EventController() {
        this.database = new EventDatabase();
    }

    /**
     * Parameterized constructor
     * @param ec
     */
    public EventController(EventController ec){
        this.database = ec.getDatabase();
    }

    /**
     * Returns the database
     * @return
     */
    public EventDatabase getDatabase() {
        return database.clone();
    }

    /**
     * Checks if the given name exists in the event
     * @param e
     * @return
     */
    public boolean validateEventNameUniqueness(Event e) {
        return database.findByName( e.getName() ) == null;
    }

    /**
     * Checks if the given name exists in the event
     * @param name
     * @return
     */
    public boolean validateEventNameUniqueness(String name) {
        return database.findByName(name) == null;
    }

    /**
     * Sets the database
     * @param database
     */
    public void setDatabase(EventDatabase database) {
        this.database = database.clone();
    }

    /**
     * Returns an ArrayList of Strings containing the name of the searched event
     * This is useful for usage with a Navigator
     * @param s
     * @return
     */
    public ArrayList<String> searchEvent(String s){
        return this.database.searchByName(s);
    }

    /**
     * Returns an event with the given name
     * @param name
     * @return
     */
    public Event getEventByName(String name){
        return this.database.findByName(name);
    }

    /**
     * Returns an event with the corresponding id
     * @param id
     * @return
     */
    public Event getEventById(int id){
        return this.database.findById(id);
    }
    
    /**
     * Adds an user to the given event
     * @param u
     * @param e
     * @throws InvalidParticipantException
     * @throws ActivityNotAvailableException
     * @throws LateForEventException
     */
    public void addUser(User u, Event e) throws InvalidParticipantException, ActivityNotAvailableException, LateForEventException{
        e.addParticipant(u);
        this.database.save(e);
    }

    /**
     * Removes the user of a given event
     * @param u
     * @param e
     * @throws InvalidParticipantException
     * @throws ActivityNotAvailableException
     * @throws InexistingUserException
     */
    public void removeUser(User u, Event e) throws InvalidParticipantException, ActivityNotAvailableException, InexistingUserException {
        e.removeParticipant(u);
        this.database.save(e);
    }
    
    /**
     * Removes the user of all the events that it participates in
     * @param u
     */
    public void removeUser(User u){
        this.database.removeUser(u);
    }

    /**
     * Saves an event to the database
     * @param e
     */
    public void addEvent(Event e){
        this.database.save(e);
    }

    @Override
    public EventController clone() {
        return new EventController(this);
    }

    @Override
    public String toString(){
        StringBuilder result = new StringBuilder();

        result.append("###Event Database###\n");
        result.append(this.database);

        return result.toString();
    }

    @Override
    public boolean equals(Object o){
        if( this == o) return true;

        if( o == null || o.getClass() != this.getClass()) return false;

        EventController ec = (EventController) o;

        return this.database.equals(ec.getDatabase());
    }

    /**
     * Returns an ArrayList containing all events
     * Useful for usage with a Navigator
     * @return 
     */
    public ArrayList<Event> getEventList() {
        return this.database.getEventList();
    }

    /**
     * Returns an ArrayList containing all upcoming events
     * Useful for usage with a Navigator
     * @return 
     */
    public ArrayList<Event> getUpcomingEvents() {
        return this.database.getUpcomingEvents();
    }

    /**
     * Returns an ArrayList containing all upcoming events of a given type
     * Useful for usage with a Navigator
     * @param type
     * @return 
     */
    public ArrayList<Event> getUpcomingEvents(String type) {
        return this.database.getUpcomingEvents(type);
    }

    /**
     * Writes the database to a file
     * @param fich
     * @throws IOException
     */
    public void writeToFile(String fich) throws IOException{
        ObjectOutputStream oos = new ObjectOutputStream( new FileOutputStream(fich) );
        oos.writeObject(this.database);
        oos.flush(); oos.close();
    }

    /**
     * Reads a event controller from a file, saving the corresponding database
     * @param fich
     * @throws IOException
     * @throws ClassNotFoundException
     */
    public void readFromFile(String fich) throws IOException, ClassNotFoundException{
        ObjectInputStream ois = new ObjectInputStream( new FileInputStream(fich) );
        EventDatabase restored = (EventDatabase) ois.readObject();
        ois.close();
        this.database = restored;
    }
}