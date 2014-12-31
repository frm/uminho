/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import java.util.ArrayList;
import java.util.Collection;
import java.util.GregorianCalendar;

/**
 *
 * @author joaorodrigues
 */
public class Project {
    private Integer id;
    private String name;
    private GregorianCalendar startDate;
    private Float budget;
    private GregorianCalendar eta;
    private GregorianCalendar endDate;
    private GregorianCalendar signDate;
    private GregorianCalendar deliveryDate;
    private Float finalCost;
    private String notes;
    private PaymentPlan paymentPlan;
    private Collection<Task> tasks;

    public Project(){}
    
    public Project(String name, Float budget, GregorianCalendar eta, String notes, PaymentPlan payment, Collection<Task> tasks) {
        this.id = -1;
        this.name = name;
        this.startDate = new GregorianCalendar();
        this.budget = budget;
        this.eta = eta;
        this.notes = notes;
        this.paymentPlan = payment;
        this.tasks = new ArrayList<Task>(tasks);
    }

    public PaymentPlan getPayment() {
        return paymentPlan;
    }

    public void setPayment(PaymentPlan payment) {
        this.paymentPlan = payment;
    }
   
    public Integer getId() {
        return id;
    }
    
    public void setId(Integer id) {
        this.id = id;
    }
    
    public String getName() {
        return name;
    }

    public GregorianCalendar getStartDate() {
        return startDate;
    }

    public Float getBudget() {
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

    public Float getFinalCost() {
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

    public void setBudget(Float budget) {
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

    public void setFinalCost(Float finalCost) {
        this.finalCost = finalCost;
    }

    public void setNotes(String notes) {
        this.notes = notes;
    }

    public PaymentPlan getPaymentPlan() {
        return paymentPlan;
    }

    public Collection<Task> getTasks() {
        return new ArrayList<Task>(tasks);
    }

    public void setPaymentPlan(PaymentPlan paymentPlan) {
        this.paymentPlan = paymentPlan;
    }

    public void setTasks(Collection<Task> tasks) {
        this.tasks = new ArrayList<Task>(tasks);
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