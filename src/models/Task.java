/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author joaorodrigues
 */
public class Task extends BasicModel {
    private String name;
    private String startDate;
    private String endDate;
    private String status;
    private int projectId;

    public Task(){
        super();
    }
    
    public Task(String name, String startDate, String endDate, String status, int projectId) {
        super(-1);
        this.name = name;
        this.startDate = startDate;
        this.endDate = endDate;
        this.status = "Em Curso";
        this.projectId = projectId;
    }
    
    public void setName(String name) {
        this.name = name;
    }

    public void setStartDate(String startDate) {
        this.startDate = startDate;
    }

    public void setEndDate(String endDate) {
        this.endDate = endDate;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public int getProjectId() {
        return projectId;
    }
    
    public String getName() {
        return name;
    }

    public String getStartDate() {
        return startDate;
    }

    public String getEndDate() {
        return endDate;
    }

    public String getStatus() {
        return status;
    }

    public void setProjectId(int projectId) {
        this.projectId = projectId;
    }

    
    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        
        if(obj == null || this.getClass() != obj.getClass())
            return false;
        
        Task t = (Task) obj;
        
        return ( super.equals(obj) );
    }
    
    
    
}
