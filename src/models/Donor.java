/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import java.util.TreeSet;

/**
 *
 * @author tiago
 */
public class Donor extends BaseEntity{
    private int id;
    private String dType;
    private String occupation;
    private String observations;
    private TreeSet<Donation> donations;

    public Donor() {
        super();
        this.id = -1;
        this.dType = "Nothing here...";
        this.occupation = "Nothing here...";
        this.observations = "Nothing here...";
        this.donations = new TreeSet();
    }
    
    public Donor(String name, String adress, String nif, String nib, TreeSet<Integer> contacts, String dType, String occupation, String observations, TreeSet<Donation> donations) {
        super(name, adress, nif, nib, contacts);
        this.id = -1;
        this.dType = dType;
        this.occupation = occupation;
        this.observations = observations;
        this.donations = new TreeSet(donations);
    }
    
    public Donor(Donor d){
        super(d);
        this.id = d.getId();
        this.dType = d.getdType();
        this.occupation = d.getOccupation();
        this.observations = d.getObservations();
        this.donations = d.getDonations();
    }

    public int getId() {
        return id;
    }

    public String getdType() {
        return dType;
    }

    public String getOccupation() {
        return occupation;
    }

    public String getObservations() {
        return observations;
    }

    public TreeSet<Donation> getDonations() {
        return new TreeSet(donations);
    }

    public void setId(int id) {
        this.id = id;
    }

    public void setdType(String dType) {
        this.dType = dType;
    }

    public void setOccupation(String occupation) {
        this.occupation = occupation;
    }

    public void setObservations(String observations) {
        this.observations = observations;
    }

    public void setDonations(TreeSet<Donation> donations) {
        this.donations = new TreeSet(donations);
    }
    
    public void addDonation(){
        //TODO
    }
    
    public Donor clone(){
        return new Donor(this);
    }
    
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("\nID: ");
        sb.append(id);
        sb.append("\nDonor Type: ");
        sb.append(dType);
        sb.append("\nOccupation: ");
        sb.append(occupation);
        sb.append("\nObservations: ");
        sb.append(observations);
        return super.toString() + sb.toString();
    }
    
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Donor d = (Donor) o;
        
        return (super.equals(o) && this.id == d.getId() && this.dType == d.getdType() && this.occupation.equals(d.getOccupation()) && this.observations.equals(d.getObservations()));
    }
    
}
