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
public class Contact extends BasicModel {
    private String type;
    private String value;
    
    public Contact() {}
    
    public Contact(String ct, String cv){
        super(-1);
        type = ct;
        value = cv;
    }

    public Contact(Contact c){
        super(c.getId());
        type = c.getType();
        value = c.getValue();
    }
    
    public String getType() {
        return type;
    }

    public String getValue() {
        return value;
    }

    public void setType(String type) {
        this.type = type;
    }

    public void setValue(String value) {
        this.value = value;
    }
    
    public void editContact(String ctype, String cvalue){
        this.setType(ctype);
        this.setValue(cvalue);
    }
    
    public Contact clone(){
        return new Contact(this);
    }
 
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder(super.toString());
        sb.append(", ");
        sb.append(type);
        sb.append(", ");
        sb.append(value);
        return sb.toString();
    }
    
    @Override
    public boolean equals(Object o) {
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Contact c = (Contact) o;
        
        return ( super.equals(o) && this.type.equals( c.getType() ) && this.value.equals( c.getValue() ) );
    }
}
