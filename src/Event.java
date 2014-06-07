/**
 *
 * @author frmendes
 */
import java.io.Serializable;
import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.Map;
import java.util.TreeMap;

public class Event implements BaseModel, Serializable {
    private int id;
    private String type;
    private UserList participants;
    private int distance;
    private EventInfo info;


    public Event() {
        this.id = -1;
        this.type = "";
        this.participants = new UserList();
        this.distance = 1;
        this.info = new EventInfo();
    }

    public Event(int id, String type, int distance, UserList participants, EventInfo info) {
        this.id = id;
        this.type = type;
        this.participants = participants.clone();
        this.info = info.clone();
        this.distance = distance;
    }

    public Event(Event e) {
        this.id = e.getId();
        this.type = e.getType();
        this.participants = e.getParticipants();
        this.info = e.getInfo();
        this.distance = e.getDistance();
    }

    public Event(String type, int distance, EventInfo info) {
        this.id = -1;
        this.type = type;
        this.participants = new UserList();
        this.distance = distance;
        this.info = info.clone();
    }

    @Override
    public void setId(int id) {
        this.id = id;
    }
    
    public void setInfo(EventInfo e) {
        this.info = e.clone();
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
    
    public EventInfo getInfo() {
        return this.info.clone();
    }

    /** Adds a new participant to the event
     * @param u User to be added
     */
    void addParticipant(User u) throws InvalidParticipantException, ActivityNotAvailableException {
        if( validForEvent(u)  )
            this.participants.addUser( u.getId() );
        else
            throw new InvalidParticipantException("User has not participated in a " + this.type + " event");
    }
    
    public boolean validForEvent(User u) throws ActivityNotAvailableException{
        return ( u.hasPracticed(this.type) && ( this.info.getCapacity() > ( this.participants.numberOfUsers() + 1) ) );
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
    
    private TreeMap<Long, User> simulateKm(int km, TreeMap<Long, User> previousKm) {
        TreeMap<Long, User> currentKm = new TreeMap<Long, User>( new SimulationComparator() );
        for( Map.Entry<Long, User> u : previousKm.entrySet() )
            currentKm.put(u.getValue().simulateKm(this.type, km, u.getKey() ), u.getValue().clone() );
        
        return currentKm;
    }
    
    private TreeMap<Long, User> simulateFirstKm(ArrayList<User> participants) {
        TreeMap<Long, User> firstKm = new TreeMap<Long, User>();
        for(User u : participants)
            firstKm.put( u.simulateKm(this.type, 1, 0), u.clone() );
        
        return firstKm;
    }

    public ArrayList< TreeMap<Long, User> > simulate(ArrayList<User> participants) {
        ArrayList< TreeMap<Long, User> > simulation = new ArrayList< TreeMap<Long, User> >();
        simulation.add(0, simulateFirstKm(participants) );
        
        for(int i = 1; i < this.distance; i++)
            simulation.add(i, simulateKm(i + 1, simulation.get(i - 1) ) );
            
        return simulation;
    }
    
    public static void printSimulatedKm(TreeMap<Long, User> sim) {
        int i = 0;
        StringBuilder sb = new StringBuilder();
        for( Map.Entry<Long, User> e : sim.entrySet() ) {
            sb.append(++i + "º: ");
            sb.append( e.getValue().getName() );
            sb.append(" Time: ");
            sb.append( StatEntry.formatMillis( e.getKey() ) );
            sb.append("\n");
        }
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

        sb.append("\nType: ");
        sb.append(this.type);
        sb.append("\nDistance: ");
        sb.append(this.distance);
        sb.append(this.info );
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
                this.info.equals( e.getInfo() ) &&
                this.participants.equals( e.getParticipants() ) &&
                this.type.equals( e.getType() )
                );
    }
}