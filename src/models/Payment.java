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
    private float cost;
    GregorianCalendar date;

    public Payment(String status, float value, GregorianCalendar date) {
        this.status = status;
        this.cost = value;
        this.date = date;
    }

    public int getId() {
        return id;
    }
    
    public void setId(int id) {
        this.id = id;
    }

    public String getStatus() {
        return status;
    }

    public float getCost() {
        return cost;
    }

    public GregorianCalendar getDate() {
        return date;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public void setCost(float cost) {
        this.cost = cost;
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
