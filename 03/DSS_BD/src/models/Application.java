/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

/**
 *
 * @author joaorodrigues
 */
public class Application extends BasicModel {
    private String applicationDate;
    private Boolean status;
    private Integer priority;
    private String notes;
    private String location;
    private String approvalDate;
    private Integer manager;
    private Integer familyId;

    public Application() {
        super();
    }

    public Application(String applicationDate, Integer priority, String notes, String location, Integer manager, Integer familyId) {
        super(-1);
        this.applicationDate = applicationDate;
        this.priority = priority;
        this.notes = notes;
        this.location = location;
        this.manager = manager;
        this.familyId = familyId;

        this.status = false;
    }

    public Integer getManager() {
        return manager;
    }
    
    public String getApplicationDate() {
        return applicationDate;
    }

    public Boolean getStatus() {
        return status;
    }

    public Integer getPriority() {
        return priority;
    }

    public String getNotes() {
        return notes;
    }

    public String getLocation() {
        return location;
    }

    public String getApprovalDate() {
        return approvalDate;
    }

    public Integer getFamilyId() {
        return familyId;
    }

    public void setFamilyId(Integer i) {
        this.familyId = i;
    }

    public void setManager(Integer i) {
        this.manager = i;
    }

    public void setApplicationDate(String applicationDate) {
        this.applicationDate = applicationDate;
    }

    public void setStatus(Boolean status) {
        this.status = status;
    }

    public void setPriority(Integer priority) {
        this.priority = priority;
    }

    public void setNotes(String notes) {
        this.notes = notes;
    }

    public void setLocation(String location) {
        this.location = location;
    }

    public void setApprovalDate(String approvalDate) {
        this.approvalDate = approvalDate;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }

        if(obj == null || this.getClass() != obj.getClass())
            return false;

        Application a = (Application) obj;

        return (a.getId() == this.getId() );
    }

}
