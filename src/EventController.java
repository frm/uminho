
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
    public static  String[] existingActivities = { "Cycling", "Kayaking", "Kendo","Running", "Skating", "Swimming"};
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


    public void setDatabase(EventDatabase database) {
        this.database = database.clone();
    }


    public ArrayList<String> searchEvent(String s) throws NoEventsAvailableException{
        return this.database.searchByName(s);
    }

    public Event getEventByName(String name){
        return this.database.findByName(name);
    }

    public void addEvent(Event e){
        this.database.save(e);
    }

    public EventController clone(){
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

    ArrayList<Event> getEventList() throws NoEventsAvailableException{
        return this.database.getEventList();
    }
}