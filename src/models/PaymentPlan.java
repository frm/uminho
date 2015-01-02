/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import java.util.ArrayList;
import java.util.Collection;

/**
 *
 * @author joaorodrigues
 */
class PaymentPlan extends BasicModel {
    private Float nextPayment;
    private String notes;
    private Collection<Payment> payments;

    public PaymentPlan(){}
    
    public PaymentPlan(Float nextPayment, String notes ) {
        super(-1);
        this.nextPayment = nextPayment;
        this.notes = notes;
        this.payments = new ArrayList<Payment>();
    }

    public Float getNextPayment() {
        return nextPayment;
    }

    public String getNotes() {
        return notes;
    }

    public void setnextPayment(Float nextPayment) {
        this.nextPayment = nextPayment;
    }

    public void setNotes(String notes) {
        this.notes = notes;
    }

    public Collection<Payment> getPayments() {
        return new ArrayList<Payment>(payments);
    }

    public void setPayments(Collection<Payment> payments) {
        this.payments = new ArrayList<Payment>();
    }
    
    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        
        if(obj == null || this.getClass() != obj.getClass())
            return false;
        
        PaymentPlan p = (PaymentPlan) obj;
        
        return ( super.equals(obj) );
    }
    
}
