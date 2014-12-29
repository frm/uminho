/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import java.util.GregorianCalendar;

/**
 *
 * @author joaorodrigues
 */
public class Project {
    private int id;
    private String name;
    private GregorianCalendar startDate;
    private float budget;
    private GregorianCalendar eta;
    private GregorianCalendar endDate;
    private GregorianCalendar signDate;
    private GregorianCalendar deliveryDate;
    private float finalCost;
    private String notes;
    private PaymentPlan paymentPlan;

    public Project(String name, float budget, GregorianCalendar eta, String notes, PaymentPlan payment) {
        this.id = -1;
        this.name = name;
        this.startDate = new GregorianCalendar();
        this.budget = budget;
        this.eta = eta;
        this.notes = notes;
        this.paymentPlan = payment;
    }

    public PaymentPlan getPayment() {
        return paymentPlan;
    }

    public void setPayment(PaymentPlan payment) {
        this.paymentPlan = payment;
    }
   
    public int getId() {
        return id;
    }
    
    public String getName() {
        return name;
    }

    public GregorianCalendar getStartDate() {
        return startDate;
    }

    public float getBudget() {
        return budget;
    }

    public GregorianCalendar getEta() {
        return eta;
    }

    public GregorianCalendar getEndDate() {
        return endDate;
    }

    public GregorianCalendar getSignDate() {
        return signDate;
    }

    public GregorianCalendar getDeliveryDate() {
        return deliveryDate;
    }

    public float getFinalCost() {
        return finalCost;
    }

    public String getNotes() {
        return notes;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setStartDate(GregorianCalendar startDate) {
        this.startDate = startDate;
    }

    public void setBudget(float budget) {
        this.budget = budget;
    }

    public void setEta(GregorianCalendar eta) {
        this.eta = eta;
    }

    public void setEndDate(GregorianCalendar endDate) {
        this.endDate = endDate;
    }

    public void setSignDate(GregorianCalendar signDate) {
        this.signDate = signDate;
    }

    public void setDeliveryDate(GregorianCalendar deliveryDate) {
        this.deliveryDate = deliveryDate;
    }

    public void setFinalCost(float finalCost) {
        this.finalCost = finalCost;
    }

    public void setNotes(String notes) {
        this.notes = notes;
    }
    
    
    
    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        
        if(obj == null || this.getClass() != obj.getClass())
            return false;
        
        Project p = (Project) obj;
        
        return (p.getId() == id );
    }
}