/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author joaorodrigues
 */
public class Family extends BasicModel  {
    private Float income;
    private String notes;
    private Boolean approved;
    private String address;
    private Representative representative;
    private List<SimpleMember> members;
    private List<Application> applications;

    public Family(){}
    
    public Family (Float income, String notes, String address, Representative representative, List<SimpleMember> members) {
        super(-1);
        this.income = income;
        this.notes = notes;
        this.address = address;
        this.representative = representative;
        this.members = members;
        
        this.approved = false;
        this.applications = new ArrayList<Application>();
    }

    public List<Application> getApplications() {
        return new ArrayList<Application>(applications);
    }

    public void setMembers(List<SimpleMember> members) {
        this.members = new ArrayList<SimpleMember>(members);
    }

    public void setApplications(List<Application> applications) {
        this.applications = new ArrayList<Application>(applications);
    }
    
    public Float getIncome() {
        return income;
    }

    public String getNotes() {
        return notes;
    }

    public Boolean isApproved() {
        return approved;
    }

    public String getAddress() {
        return address;
    }

    public Representative getRepresentative() {
        return representative;
    }

    public void setIncome(Float income) {
        this.income = income;
    }

    public void setNotes(String notes) {
        this.notes = notes;
    }

    public void setApproved(Boolean approved) {
        this.approved = approved;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public void setRepresentative(Representative representative) {
        this.representative = representative;
    }

    public List<SimpleMember> getMembers() {
        return members;
    }
    
       @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        
        if(obj == null || this.getClass() != obj.getClass())
            return false;
        
        Family f = (Family) obj;
        
        return ( super.equals(obj) );
    }

}
