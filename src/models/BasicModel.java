/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package models;

/**
 *
 * @author frmendes
 */
public class BasicModel {
    private Integer id;
    
    public BasicModel() {}
    
    public BasicModel(Integer id) {
        this.id = new Integer(id);
    }
    
    public BasicModel(int id) {
        this.id = new Integer(id);
    }
    
    public BasicModel(BasicModel b) {
        this.id = b.getId();
    }
    
    public void setId(Integer id) {
        this.id = new Integer(id);
    }
    
    public void setId(int id) {
        this.id = id;
    }
    
    public int getId() {
        return this.id;
    }
    
    public int hashCode()  {
        return id;
    }
    
    public BasicModel clone() {
        return new BasicModel(this);
    }
    
    public String toString() {
        return id.toString();
    }
    
    public boolean equals(Object o) {
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
        
        BasicModel b = (BasicModel) o;
        return this.id == b.getId();
    }
}
