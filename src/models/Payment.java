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
class Payment {
    private int id;
    private String status;
    private float value;
    GregorianCalendar date;

    public Payment(String status, float value, GregorianCalendar date) {
        this.status = status;
        this.value = value;
        this.date = date;
    }

    public int getId() {
        return id;
    }

    public String getStatus() {
        return status;
    }

    public float getValue() {
        return value;
    }

    public GregorianCalendar getDate() {
        return date;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public void setValue(float value) {
        this.value = value;
    }

    public void setDate(GregorianCalendar date) {
        this.date = date;
    }
    
    
            
       @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        
        if(obj == null || this.getClass() != obj.getClass())
            return false;
        
        Payment p = (Payment) obj;
        
        return (p.getId() == id );
    }
}
