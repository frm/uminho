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
    private Boolean type;
    private String observations;
    private HashSet<Donation> donations;
    private String lastDonationDate;

    public Donor() {
        super();
    }
    
    public Donor(String name, String address, String nif, String nib, Activity activity, HashSet<Contact> contacts, Boolean dType, String observations) {
        super(name, address, nif, nib, activity, contacts);
        this.type = dType;
        this.observations = observations;
        this.donations = new HashSet();
        this.lastDonationDate = null;
    }
    
    public Donor(Donor d){
        super(d);
        this.type = d.getType();
        this.observations = d.getObservations();
        this.donations = d.getDonations();
        this.lastDonationDate = d.getLastDonationDate();
    }

    public Boolean getType() {
        return type;
    }

    public String getObservations() {
        return observations;
    }

    public HashSet<Donation> getDonations() {
        return new HashSet(donations);
    }
    
    public String getLastDonationDate() {
        return lastDonationDate;
    }

    public void setType(Boolean type) {
        this.type = type;
    }

    public void setObservations(String observations) {
        this.observations = observations;
    }

    public void setDonations(HashSet<Donation> donations) {
        this.donations = new HashSet(donations);
    }
    
    public void setLastDonationDate(String lastDonationDate) {
        this.lastDonationDate = lastDonationDate;        
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
        StringBuilder sb = new StringBuilder(super.toString());
        sb.append(", ");
        sb.append(type);
        sb.append(", ");
        sb.append(observations);
        return sb.toString();
    }
    
    @Override
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Donor d = (Donor) o;
        
        return (super.equals(o) && this.type == d.getType() && this.observations.equals(d.getObservations()));
    }
    
}
