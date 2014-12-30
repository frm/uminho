/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import java.util.GregorianCalendar;
import java.util.HashSet;

/**
 *
 * @author tiago
 */
public class Event {
    private int id;
    private GregorianCalendar date;
    private float amountRaised;
    private int participantsNr;
    private String location;
    private String observations;
    private HashSet<Integer> volunteers;

    public Event() {
        this.id = -1;
        this.date = new GregorianCalendar();
        this.amountRaised = -1;
        this.participantsNr = -1;
        this.location = "Nothing here...";
        this.observations = "Nothing here...";
        this.volunteers = new HashSet<Integer>();
    }
    
    public Event(GregorianCalendar date, float amountRaised, int participantsNr, String location, String observations, HashSet<Integer> volunteers) {
        this.id = -1;
        this.date = date;
        this.amountRaised = amountRaised;
        this.participantsNr = participantsNr;
        this.location = location;
        this.observations = observations;
        this.volunteers = new HashSet(volunteers);
    }
    
    public Event(Event e) {
        this.id = e.getId();
        this.date = e.getDate();
        this.amountRaised = e.getAmountRaised();
        this.participantsNr = e.getParticipantsNr();
        this.location = e.getLocation();
        this.observations = e.getObservations();
    }

    public int getId() {
        return id;
    }
    
    public GregorianCalendar getDate() {
        return date;
    }

    public float getAmountRaised() {
        return amountRaised;
    }

    public int getParticipantsNr() {
        return participantsNr;
    }

    public String getLocation() {
        return location;
    }

    public String getObservations() {
        return observations;
    }

    public HashSet<Integer> getVolunteers() {
        return new HashSet(volunteers);
    }

    public void setDate(GregorianCalendar date) {
        this.date = date;
    }

    public void setAmountRaised(float amountRaised) {
        this.amountRaised = amountRaised;
    }

    public void setParticipantsNr(int participantsNr) {
        this.participantsNr = participantsNr;
    }

    public void setLocation(String location) {
        this.location = location;
    }

    public void setObservations(String observations) {
        this.observations = observations;
    }

    public void setVolunteers(HashSet<Integer> volunteers) {
        this.volunteers = volunteers;
    }
    
    @Override
     public Event clone(){
        return new Event(this);
    }
     
    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder();
        
        sb.append("\n");
        sb.append(id);
        sb.append(", ");
        sb.append(date);
        sb.append(", ");
        sb.append(amountRaised);
        sb.append(", ");
        sb.append(participantsNr);
        sb.append(", ");
        sb.append(location);
        sb.append(", ");
        sb.append(observations);
        
        return sb.toString();
    }
    
    @Override
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Event e = (Event) o;
        
        return (this.date.equals(e.getDate()) && this.amountRaised == e.getAmountRaised() && this.participantsNr == e.getParticipantsNr() && this.location.equals(e.getLocation()) && this.observations.equals(e.getObservations()));
    }
}
