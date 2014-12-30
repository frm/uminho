/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

/**
 *
 * @author tiago
 */
public class Contact {
    private int cId;
    private String cType;
    private String cValue;
    
    public Contact(){
        cId = -1;
        cType = "Nothing here...";
        cValue = "Nothing here...";
    }
    
    public Contact(String ct, String cv){
        cId = -1;
        cType = ct;
        cValue = cv;
    }

    public Contact(Contact c){
        cId = c.getcId();
        cType = c.getcType();
        cValue = c.getcValue();
    }

    public int getcId() {
        return cId;
    }
    
    public String getcType() {
        return cType;
    }

    public String getcValue() {
        return cValue;
    }

    public void setcId(int cId) {
        this.cId = cId;
    }

    public void setcType(String cType) {
        this.cType = cType;
    }

    public void setcValue(String cValue) {
        this.cValue = cValue;
    }
    
    public void editContact(String ctype, String cvalue){
        this.setcType(ctype);
        this.setcValue(cvalue);
    }
    
    public Contact clone(){
        return new Contact(this);
    }
}