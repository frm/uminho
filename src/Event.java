/**
 *
 * @author frmendes
 */
import java.util.GregorianCalendar;

public class Event implements BaseModel {
    private int id;
    private String type;
    private UserList participants;
    private int distance;
    private int altitude;
    private EventInfo info;


    public Event() {
        this.id = -1;
        this.type = "";
        this.participants = new UserList();
        this.distance = 1;
        this.altitude = 1;
        this.info = new EventInfo();
    }

    public Event(int id, String type, int distance, int altitude, UserList participants, EventInfo info) {
        this.id = id;
        this.type = type;
        this.participants = participants.clone();
        this.info = info.clone();
        this.distance = distance;
        this.altitude = altitude;
    }

    public Event(Event e) {
        this.id = e.getId();
        this.type = e.getType();
        this.participants = e.getParticipants();
        this.info = e.getInfo();
        this.distance = e.getDistance();
        this.altitude = e.getAltitude();
    }

    public Event(String name, String type, int distance, int altitude, EventInfo info) {
        this.id = -1;
        this.type = type;
        this.participants = new UserList();
        this.distance = distance;
        this.altitude = altitude;
        this.info = info.clone();
    }

    @Override
    public void setId(int id) {
        this.id = id;
    }
    
    public void setInfo(EventInfo e) {
        this.info = e.clone();
    }
    
    public void setAltitude(int a) {
        this.altitude = a;
    }
    
    public void setDistance(int d) {
        this.distance = d;
    }

    /** Sets the participants
     * @param p UserList of participants
     */
    public void setParticipants(UserList p) {
        this.participants = p.clone();
    }

    /** Sets the type
     * @param type type of the event
     */
    public void setType(String type) {
        this.type = type;
    }

    @Override
    public int getId() {
        return this.id;
    }

    /** Returns the type of event
     * @return type of the event
     */
    public String getType() {
        return this.type;
    }

    /** Returns the name of the event
     * @return name of the event
     */
    public String getName() {
        return this.info.getName();
    }

    /** Returns the maximum number of users allowed
     * @return capacity of the event
     */
    public int getCapacity() {
        return this.info.getCapacity();
    }

    /** Returns the expected weather for the event
     * @return event weather
     */
    public String getWeather() {
        return this.info.getWeather();
    }

    /** Returns the list of users participating
     * @return participating user list
     */
    public UserList getParticipants() {
        return this.participants.clone();
    }
    

    /** Returns the date of the event
     * @return date of the event
     */
    public GregorianCalendar getDate() {
        return this.info.getDate();
    }
    
    public int getDistance() {
        return this.distance;
    }
    
    public int getAltitude() {
        return this.altitude;
    }
    
    public EventInfo getInfo() {
        return this.info.clone();
    }

    /** Adds a new participant to the event
     * @param u User to be added
     */
    void addParticipant(User u) throws InvalidParticipantException, ActivityNotAvailableException {
        if( u.hasPracticed(this.type) )
            this.participants.addUser( u.getId() );
        else
            throw new InvalidParticipantException("User has not participated in a " + this.type + " event");
    }

    /** Removes a participant from the event
     * Shall throw a InexistingUserException if the user does not participate in the event
     * @param u User to be removed
     * @throws InexistingUserException
     */
    void removeParticipant(User u) throws InexistingUserException {
        if(! this.participants.removeUser( u.getId() ) )
            throw new InexistingUserException("User does not exist");
    }


    /** Removes a participant from the event
     * Shall throw a InexistingUserException if the user does not participate in the event
     * @param id ID of the User to be removed
     * @throws InexistingUserException
     */
    void removeParticipant(int id) throws InexistingUserException {
        if(! this.participants.removeUser(id) )
            throw new InexistingUserException("User does not exist");
    }
    
    public boolean isUpcoming() {
        return info.isUpcoming();
    }
    

    @Override
    public int hashCode() {
        return new Integer( this.getId() ).hashCode();
    }

    @Override
    public Event clone() {
        return new Event(this);
    }

    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder();

        sb.append("### Event ###\n");
        sb.append("Id: ");
        sb.append(this.id);
        sb.append("\nType: ");
        sb.append(this.type);
        sb.append("\nName: ");
        sb.append(this.name);
        sb.append("\nDate");
        sb.append(this.date);
        sb.append("\nWeather Prediction: ");
        sb.append(this.weather);
        sb.append("\nMax entries: ");
        sb.append(this.capacity);
        sb.append("\nParticipants: ");
        sb.append(this.participants);

        return sb.toString();
    }

    @Override
    public boolean equals(Object o) {
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass()) return false;

        Event e = (Event) o;

        return (
                this.id == e.getId() &&
                this.distance == e.getDistance() &&
                this.altitude == e.getAltitude() &&
                this.info.equals( e.getInfo() ) &&
                this.participants.equals( e.getParticipants() ) &&
                this.type.equals( e.getType() )
                );
    }
}