/**
 *
 * @author frmendes
 */
import java.util.HashSet;

<<<<<<< HEAD
/**
 *
 * @author joaorodrigues
 */

public class Event implements BaseModel {
    private int id;
    private int capacity;
    private String name;
    private Weather weather;
    private UserList participants;

    /** Default number of participants, given in case the number is not valid or given */
    private static final int defaultParticipants = Integer.MAX_VALUE;


    public Event(){
        this.id = -1;
        this.name = "";
        this.capacity = Event.defaultParticipants;
        this.weather = new Weather();
        this.participants = new UserList();
    }

    public Event(int id, String name, int capacity, String weather, UserList participants){
        this.id = id;
        this.name = name;
        this.setCapacity(capacity);
        this.weather = new Weather(weather);
        this.participants = participants.clone();
    }

    /**
     *
     * @param id
     * @param name
     * @param capacity
     * @param weather
     * @param participants
     */
    public Event(int id, String name, int capacity, Weather weather, UserList participants) {
        this.id = id;
        this.name = name;
        this.setCapacity(capacity);
        this.weather = new Weather(weather);
        this.participants = participants.clone();
    }

    /**
     *
     * @param e
     */
    public Event(Event e){
        this.id = e.getId();
        this.name = e.getName();
        this.capacity = e.getCapacity();
        this.weather = new Weather (e.getWeather());
        this.participants = e.getParticipants();
    }

    @Override
    public void setId(int id) {
        this.id = id;
    }

    /** Sets the event name
     * @param s name of the event
      */
    public void setName(String s) {
        this.name = s;
    }

    /** Sets the event capacity
     * It shall fallback to Event.defaultParticipants in case the given number is < 0
     * @param c capacity
     */
    public void setCapacity(int c) {
        if (c > 0)
            this.capacity = c;
        else
            this.capacity = Event.defaultParticipants;
    }

    /** Sets the expected weather for the event
     * @param w expected weather
     */
    public void setWeather(int w) {
        this.weather.setWeather(w);
    }

    /** Sets the participants
     * @param p UserList of participants
     */
    public void setParticipants(UserList p) throws InvalidUserListException {
        this.participants = p.clone();
    }

    @Override
    public int getId() {
        return this.id;
    }

    /** Returns the name of the event
     * @return name of the event
     */
    public String getName() {
        return this.name;
    }

    /** Returns the maximum number of users allowed
     * @return capacity of the event
     */
    public int getCapacity() {
        return this.capacity;
    }

    /** Returns the expected weather for the event
     * @return event weather
     */
    public String getWeather() {
        return this.weather.getWeather();
    }

    /** Returns the list of users participating
     * @return participating user list
     */
    public UserList getParticipants() {
        return this.participants.clone();
    }

    /** Adds a new participant to the event
     * @param u User to be added
     */
    void addParticipant(User u) {
        this.participants.addUser( u.getId() );
    }

     /** Adds a new participant to the event
      * @param id id of the user that will be added
      */
    void addParticipant(int id) {
        this.participants.addUser(id);
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
        sb.append("\nName: ");
        sb.append(this.name);
        sb.append("\nWeather Prediction: ");
        sb.append(this.weather);
        sb.append("\nMax entries: ");
        sb.append(this.capacity);
        sb.append("\nParticipants: ");
        sb.append(this.participants);

        return sb.toString();
    }

    @Override
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass()) return false;

        Event e = (Event) o;

        return(
                this.id == e.getId() &&
                this.name.equals(e.getName()) &&
                this.weather.getWeather().equals(e.getWeather()) &&
                this.capacity == e.getCapacity() &&
                this.participants.equals( e.getParticipants() ) );
    }
}
