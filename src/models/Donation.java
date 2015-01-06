/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;


/**
 *
 * @author tiago
 */
public class Donation extends BasicModel {
    private Integer materialID;
    private String donationType;
    private String donationDate;
    private Integer quantity;
    private Double amount;
    private Boolean used;
    private String observations;

    public Donation() {
        super();
    }
    
    public Donation(Integer materialID, String donationType, String donationDate, Integer quantity, Double amount, Boolean used, String observations) {
        super(-1);
        this.materialID=materialID;
        this.donationType = donationType;
        this.donationDate = donationDate;
        this.quantity = quantity;
        this.amount = amount;
        this.used=used;
        this.observations = observations;
    }
    
    public Donation(Donation don){
        super( don.getId() );
        this.materialID = don.getMaterialID();
        this.donationType = don.getDonationType();
        this.donationDate = don.getDonationDate();
        this.quantity = don.getQuantity();
        this.amount = don.getAmount();
        this.used=don.getUsed();
        this.observations = don.getObservations();
    }

    public Integer getMaterialID() {
        return materialID;
    }
    
    public String getDonationType() {
        return donationType;
    }

    public String getDonationDate() {
        return donationDate;
    }

    public Integer getQuantity() {
        return quantity;
    }

    public Double getAmount() {
        return amount;
    }
    
    public Boolean getUsed() {
        return used;
    }

    public String getObservations() {
        return observations;
    }

    public void setMaterialID(Integer id){
        this.materialID=id;
    }
    public void setDonationType(String donationType) {
        this.donationType = donationType;
    }

    public void setDonationDate(String donationDate) {
        this.donationDate = donationDate;
    }

    public void setQuantity(Integer quantity) {
        this.quantity = quantity;
    }

    public void setAmount(Double amount) {
        this.amount = amount;
    }
    
    public void setUsed(Boolean used) {
        this.used=used;
    }

    public void setObservations(String observations) {
        this.observations = observations;
    }
    
    @Override
    public Donation clone(){
        return new Donation(this);
    }
    

    
    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder(super.toString());
        sb.append(", ");
        sb.append(donationType);
        sb.append(", ");
        sb.append(donationDate);
        sb.append(", ");
        sb.append(amount);
        sb.append(", ");
        sb.append(quantity);
        return sb.toString();
    }
    
    @Override
    public boolean equals (Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Donation don = (Donation) o;
        
        return ( super.equals(o) && this.donationType.equals( don.getDonationType() ) && this.donationDate.equals(don.getDonationDate()) && this.amount == don.getAmount() && this.quantity == don.getQuantity() );
    }
}
