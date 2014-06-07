
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;



/**
 *
 * @author joaorodrigues
 */
public class EventController implements Serializable{
    private EventDatabase database;

    /**
     *
     */
    public EventController() {
        this.database = new EventDatabase();
    }

    /**
     *
     * @param ec
     */
    public EventController(EventController ec){
        this.database = ec.getDatabase();
    }

    /**
     *
     * @return
     */
    public EventDatabase getDatabase() {
        return database.clone();
    }

    /**
     *
     * @param e
     * @return
     */
    public boolean validateEventNameUniqueness(Event e) {
        return database.findByName( e.getName() ) == null;
    }

    /**
     *
     * @param name
     * @return
     */
    public boolean validateEventNameUniqueness(String name) {
        return database.findByName(name) == null;
    }

    /**
     *
     * @param database
     */
    public void setDatabase(EventDatabase database) {
        this.database = database.clone();
    }

    /**
     *
     * @param s
     * @return
     */
    public ArrayList<String> searchEvent(String s){
        return this.database.searchByName(s);
    }

    /**
     *
     * @param name
     * @return
     */
    public Event getEventByName(String name){
        return this.database.findByName(name);
    }

    /**
     *
     * @param id
     * @return
     */
    public Event getEventById(int id){
        return this.database.findById(id);
    }
    
    /**
     *
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
     *
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
     *
     * @param u
     */
    public void removeUser(User u){
        this.database.removeUser(u);
    }

    /**
     *
     * @param e
     */
    public void addEvent(Event e){
        this.database.save(e);
    }

    public EventController clone() {
        return new EventController(this);
    }

    public String toString(){
        StringBuilder result = new StringBuilder();

        result.append("###Event Database###\n");
        result.append(this.database);

        return result.toString();
    }

    public boolean equals(Object o){
        if( this == o) return true;

        if( o == null || o.getClass() != this.getClass()) return false;

        EventController ec = (EventController) o;

        return this.database.equals(ec.getDatabase());
    }

    ArrayList<Event> getEventList() {
        return this.database.getEventList();
    }

    ArrayList<Event> getUpcomingEvents() {
        return this.database.getUpcomingEvents();
    }

    ArrayList<Event> getUpcomingEvents(String name) {
        return this.database.getUpcomingEvents(name);
    }

    /**
     *
     * @param fich
     * @throws IOException
     */
    public void writeToFile(String fich) throws IOException{
        ObjectOutputStream oos = new ObjectOutputStream( new FileOutputStream(fich) );
        oos.writeObject(this.database);
        oos.flush(); oos.close();
    }

    /**
     *
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