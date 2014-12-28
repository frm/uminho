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
    int id;
    float income;
    String notes;
    boolean approved;
    String address;
    Representative representative;
    List<SimpleMember> members;

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
}
