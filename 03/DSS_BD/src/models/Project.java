/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

/**
 *
 * @author joaorodrigues
 */
public class Project extends BasicModel {
    private String name;
    private String startDate;
    private Float budget;
    private String eta;
    private String endDate;
    private String signDate;
    private String deliveryDate;
    private Float finalCost;
    private String notes;
    private int applicationId;
    private int paymentPlanId;

    public Project(){
        super();
    }

    public Project(String name, String startDate, Float budget, String eta, String notes, int aId, int ppId) {
        super(-1);
        this.name = name;
        this.startDate = startDate;
        this.budget = budget;
        this.eta = eta;
        this.notes = notes;
        this.applicationId = aId;
        this.paymentPlanId = ppId;
    }

    public String getName() {
        return name;
    }

    public String getStartDate() {
        return startDate;
    }

    public Float getBudget() {
        return budget;
    }

    public String getEta() {
        return eta;
    }

    public String getEndDate() {
        return endDate;
    }

    public String getSignDate() {
        return signDate;
    }

    public String getDeliveryDate() {
        return deliveryDate;
    }

    public Float getFinalCost() {
        return finalCost;
    }

    public String getNotes() {
        return notes;
    }

    public int getApplicationId() {
        return applicationId;
    }

    public int getPaymentPlanId() {
        return paymentPlanId;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setStartDate(String startDate) {
        this.startDate = startDate;
    }

    public void setBudget(Float budget) {
        this.budget = budget;
    }

    public void setEta(String eta) {
        this.eta = eta;
    }

    public void setEndDate(String endDate) {
        this.endDate = endDate;
    }

    public void setSignDate(String signDate) {
        this.signDate = signDate;
    }

    public void setDeliveryDate(String deliveryDate) {
        this.deliveryDate = deliveryDate;
    }

    public void setFinalCost(Float finalCost) {
        this.finalCost = finalCost;
    }

    public void setNotes(String notes) {
        this.notes = notes;
    }

    public void setApplicationId(int applicationId) {
        this.applicationId = applicationId;
    }

    public void setPaymentPlanId(int paymentPlanId) {
        this.paymentPlanId = paymentPlanId;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }

        if(obj == null || this.getClass() != obj.getClass())
            return false;

        Project p = (Project) obj;

        return ( super.equals(obj) );
    }
}