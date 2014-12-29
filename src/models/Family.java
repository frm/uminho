/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import java.util.List;

/**
 *
 * @author joaorodrigues
 */
public class Family{
    private int id;
    private float income;
    private String notes;
    private boolean approved;
    private String address;
    private Representative representative;
    private List<SimpleMember> members;

    public float getIncome() {
        return income;
    }

    public String getNotes() {
        return notes;
    }

    public boolean isApproved() {
        return approved;
    }

    public String getAddress() {
        return address;
    }

    public Representative getRepresentative() {
        return representative;
    }

    public void setIncome(float income) {
        this.income = income;
    }

    public void setNotes(String notes) {
        this.notes = notes;
    }

    public void setApproved(boolean approved) {
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

    public int getId() {
        return id;
    }
    
       @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        
        if(obj == null || this.getClass() != obj.getClass())
            return false;
        
        Family f = (Family) obj;
        
        return (f.getId() == id );
    }

}
