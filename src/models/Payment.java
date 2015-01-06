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
public class Payment extends BasicModel {
    private String status;
    private Float cost;
    private String date;
    private int paymentPlanId;

    public Payment(){
        super();
    }
    
    public Payment(String status, Float value, String date, int ppId) {
        super(-1);
        this.status = status;
        this.cost = value;
        this.date = date;
        this.paymentPlanId = ppId;
    }

    public String getStatus() {
        return status;
    }

    public Float getCost() {
        return cost;
    }

    public String getDate() {
        return date;
    }

    public int getPaymentPlanId() {
        return paymentPlanId;
    }
    
    public void setStatus(String status) {
        this.status = status;
    }

    public void setCost(Float cost) {
        this.cost = cost;
    }

    public void setDate(String date) {
        this.date = date;
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
        
        Payment p = (Payment) obj;
        
        return ( super.equals(obj) );
    }
}
