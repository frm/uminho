
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author joaorodrigues
 */
public class EventController {
    private EventDatabase database;

    public EventController() {
        this.database = new EventDatabase();
    }

    public EventController(EventController ec){
        this.database = ec.getDatabase();
    }

    public EventDatabase getDatabase() {
        return database.clone();
    }

    public boolean validateEventNameUniqueness(Event e) {
        return database.findByName( e.getName() ) == null;
    }

    public boolean validateEventNameUniqueness(String name) {
        return database.findByName(name) == null;
    }

    public void setDatabase(EventDatabase database) {
        this.database = database.clone();
    }

    public ArrayList<String> searchEvent(String s){
        return this.database.searchByName(s);
    }

    public Event getEventByName(String name){
        return this.database.findByName(name);
    }

    public Event getEventById(int id){
        return this.database.findById(id);
    }

    public void addUser(User u, Event e) throws InvalidParticipantException, ActivityNotAvailableException{
        e.addParticipant(u);
        this.database.save(e);
    }

    public void removeUser(User u, Event e) throws InvalidParticipantException, ActivityNotAvailableException, InexistingUserException {
        e.removeParticipant(u);
        this.database.save(e);
    }
    
    public void removeUser(User u){
        this.database.removeUser(u);
    }

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

    public void writeToFile(String fich) throws IOException{
        ObjectOutputStream oos = new ObjectOutputStream( new FileOutputStream(fich) );
        oos.writeObject(this.database);
        oos.flush(); oos.close();
    }

    public void readFromFile(String fich) throws IOException, ClassNotFoundException{
        ObjectInputStream ois = new ObjectInputStream( new FileInputStream(fich) );
        EventDatabase restored = (EventDatabase) ois.readObject();
        ois.close();
        this.database = restored;
    }
}