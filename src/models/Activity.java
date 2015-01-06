/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package models;

/**
 *
 * @author mendes
 */
public class Activity extends BasicModel {
    String name;
    public Activity() {
        super();
    }
    
    public Activity(String name) {
        super(-1);
        this.name = name;
    }
    
    public String getName() {
        return this.name;
    }
    
    public void setName(String n) {
        this.name = n;
    }
    
    public String toString(){
        return name;
    }
}
