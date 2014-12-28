/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import java.util.GregorianCalendar;

/**
 *
 * @author tiago
 */
public class Donation {
    private int id;
    private int donationType;
    private GregorianCalendar donationDate;
    private int quantity;
    private double amount;

    public Donation() {
        this.id = -1;
        this.donationType = -1;
        this.donationDate = new GregorianCalendar();
        this.quantity = -1;
        this.amount = -1.0;
    }
    
    public Donation(int donationType, GregorianCalendar dontationDate, int quantity, double amount) {
        this.id = -1;
        this.donationType = donationType;
        this.donationDate = dontationDate;
        this.quantity = quantity;
        this.amount = amount;
    }
    
    public Donation(Donation don){
        this.id = don.getId();
        this.donationType = don.getDonationType();
        this.donationDate = don.getDonationDate();
        this.quantity = don.getQuantity();
        this.amount = don.getAmount();
    }

    public int getId() {
        return id;
    }

    public int getDonationType() {
        return donationType;
    }

    public GregorianCalendar getDonationDate() {
        return donationDate;
    }

    public int getQuantity() {
        return quantity;
    }

    public double getAmount() {
        return amount;
    }

    public void setId(int id) {
        this.id = id;
    }

    public void setDonationType(int donationType) {
        this.donationType = donationType;
    }

    public void setDonationDate(GregorianCalendar dontationDate) {
        this.donationDate = dontationDate;
    }

    public void setQuantity(int quantity) {
        this.quantity = quantity;
    }

    public void setAmount(double amount) {
        this.amount = amount;
    }
    
    @Override
    public Donation clone(){
        return new Donation(this);
    }
    
    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("ID: ");
        sb.append(id);
        sb.append("Tipo de doação");
        sb.append(donationType);
        sb.append("Data doação: ");
        sb.append(donationDate);
        sb.append("Valor: ");
        sb.append(amount);
        sb.append("Quantidade: ");
        sb.append(quantity);
        return sb.toString();
    }
    
    @Override
    public boolean equals (Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Donation don = (Donation) o;
        
        return (this.id == don.getId() && this.donationType == don.getDonationType() && this.donationDate.equals(don.getDonationDate()) && this.amount == don.getAmount() && this.quantity == don.getQuantity());
    }
}
