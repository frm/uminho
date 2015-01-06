/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author joaorodrigues
 */
public class Family extends BasicModel  {
    private String name;
    private String address;
    private Float income;
    private Integer volunteerHours;
    private Boolean approved;
    private String observations;

    public Family() {
        super();
    }
    
    public Family (String name, String address, Float income, String observations) {
        super(-1);
        this.income = income;
        this.observations = observations;
        this.address = address;
        this.name = name;
        
        this.approved = false;
        this.volunteerHours = 0;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Integer getVolunteerHours() {
        return volunteerHours;
    }

    public void setVolunteerHours(Integer volunteerHours) {
        this.volunteerHours = volunteerHours;
    }
    
    public Float getIncome() {
        return income;
    }

    public String getObservations() {
        return observations;
    }

    public Boolean getApproved() {
        return approved;
    }

    public String getAddress() {
        return address;
    }

    public void setIncome(Float income) {
        this.income = income;
    }

    public void setObservations(String observations) {
        this.observations = observations;
    }

    public void setApproved(Boolean approved) {
        this.approved = approved;
    }

    public void setAddress(String address) {
        this.address = address;
    }
    
    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        
        if(obj == null || this.getClass() != obj.getClass())
            return false;
        
        Family f = (Family) obj;
        
        return ( super.equals(obj) );
    }

}
