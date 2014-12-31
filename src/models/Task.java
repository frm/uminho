/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author joaorodrigues
 */
class Task {
    private int id;
    private String name;
    private GregorianCalendar startDate;
    private GregorianCalendar endDate;
    private String status;
    private int hours;
    private Map<Volunteer, Integer> volunteers;

    public Task(String name, GregorianCalendar startDate, GregorianCalendar endDate, Map<Volunteer, Integer> volunteers) {
        this.id = -1;
        this.name = name;
        this.startDate = startDate;
        this.endDate = endDate;
        this.status = "Em Curso";
        this.volunteers = volunteers;
    }

    public void setId(int id) {
        this.id = id;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setStartDate(GregorianCalendar startDate) {
        this.startDate = startDate;
    }

    public void setEndDate(GregorianCalendar endDate) {
        this.endDate = endDate;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public void setHours(int hours) {
        this.hours = hours;
    }

    public void setVolunteers(Map<Volunteer, Integer> volunteers) {
        this.volunteers = new HashMap<Volunteer, Integer>(volunteers);
    }

    public int getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public GregorianCalendar getStartDate() {
        return startDate;
    }

    public GregorianCalendar getEndDate() {
        return endDate;
    }

    public String getStatus() {
        return status;
    }

    public int getHours() {
        return hours;
    }

    public Map<Volunteer, Integer> getVolunteers() {
        return new HashMap<Volunteer, Integer>(volunteers);
    }
    
    
    
    
    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        
        if(obj == null || this.getClass() != obj.getClass())
            return false;
        
        Task t = (Task) obj;
        
        return (t.getId() == id );
    }
    
    
    
}
