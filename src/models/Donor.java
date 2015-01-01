/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import java.util.HashSet;

/**
 *
 * @author tiago
 */
public class Donor extends BaseEntity{
    private Integer id;
    private String type;
    private String occupation;
    private String observations;
    private HashSet<Donation> donations;

    public Donor() {
        super();
    }
    
    public Donor(String name, String adress, String nif, String nib, String activity, HashSet<Contact> contacts, String dType, String occupation, String observations, HashSet<Donation> donations) {
        super(name, adress, nif, nib, activity, contacts);
        this.id = -1;
        this.type = dType;
        this.occupation = occupation;
        this.observations = observations;
        this.donations = new HashSet(donations);
    }
    
    public Donor(Donor d){
        super(d);
        this.id = d.getId();
        this.type = d.getType();
        this.occupation = d.getOccupation();
        this.observations = d.getObservations();
        this.donations = d.getDonations();
    }

    public Integer getId() {
        return id;
    }
    
    public void setId(Integer id) {
        this.id = id;
    }

    public String getType() {
        return type;
    }

    public String getOccupation() {
        return occupation;
    }

    public String getObservations() {
        return observations;
    }

    public HashSet<Donation> getDonations() {
        return new HashSet(donations);
    }

    public void setType(String type) {
        this.type = type;
    }

    public void setOccupation(String occupation) {
        this.occupation = occupation;
    }

    public void setObservations(String observations) {
        this.observations = observations;
    }

    public void setDonations(HashSet<Donation> donations) {
        this.donations = new HashSet(donations);
    }
    
    public void addDonation(Donation d){
        this.donations.add(d);
    }
    
    @Override
    public Donor clone(){
        return new Donor(this);
    }
    
    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("\n");
        sb.append(id);
        sb.append(", ");
        sb.append(type);
        sb.append(", ");
        sb.append(occupation);
        sb.append(", ");
        sb.append(observations);
        return super.toString() + sb.toString();
    }
    
    @Override
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Donor d = (Donor) o;
        
        return (super.equals(o) && this.type == d.getType() && this.occupation.equals(d.getOccupation()) && this.observations.equals(d.getObservations()));
    }
    
}
