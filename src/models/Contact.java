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
    
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("\n");
        sb.append(cId);
        sb.append(", ");
        sb.append(cType);
        sb.append(", ");
        sb.append(cValue);
        return sb.toString();
    }
    
    public boolean equals(Object o) {
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Contact c = (Contact) o;
        
        return (this.cType == c.getcType() && this.cValue == c.getcValue());
    }
}
