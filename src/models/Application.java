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
public class Application {
    int id;
    GregorianCalendar applicationDate;
    String file;
    boolean status;
    String priority;
    String notes;
    String location;
    GregorianCalendar approvalDate;
    Map<Integer, String> questionnaire;
    Project projeto;

    public Application(GregorianCalendar applicationDate, String file, String priority, String notes, String location, Map<Integer, String> questionnaire) {
        this.applicationDate = applicationDate;
        this.file = file;
        this.priority = priority;
        this.notes = notes;
        this.location = location;
        this.questionnaire = questionnaire;
        
        this.id = -1;
        this.status = false;
        this.applicationDate = new GregorianCalendar();
        this.projeto = null;
    }
    
    
    

    public Application(String location, Map<Integer, String> questionnaire) {
        int id = -1;
        this.status = false;
        this.file = null;
        this.applicationDate = new GregorianCalendar();
        this.priority = "Normal";
        
        this.location = location;
        this.questionnaire = questionnaire;
    }

    public int getId() {
        return id;
    }

    public GregorianCalendar getApplicationDate() {
        return applicationDate;
    }

    public String getFile() {
        return file;
    }

    public boolean getStatus() {
        return status;
    }

    public String getPriority() {
        return priority;
    }

    public String getNotes() {
        return notes;
    }

    public String getLocation() {
        return location;
    }

    public GregorianCalendar getApprovalDate() {
        return approvalDate;
    }

    public Map<Integer, String> getQuestionnaire() {
        return questionnaire;
    }

    public void setApplicationDate(GregorianCalendar applicationDate) {
        this.applicationDate = applicationDate;
    }

    public void setFile(String file) {
        this.file = file;
    }

    public void setStatus(boolean status) {
        this.status = status;
    }

    public void setPriority(String priority) {
        this.priority = priority;
    }

    public void setNotes(String notes) {
        this.notes = notes;
    }

    public void setLocation(String location) {
        this.location = location;
    }

    public void setApprovalDate(GregorianCalendar approvalDate) {
        this.approvalDate = approvalDate;
    }

    public void setQuestionnaire(Map<Integer, String> questionnaire) {
        this.questionnaire = questionnaire;
    }
    
    
    
}
