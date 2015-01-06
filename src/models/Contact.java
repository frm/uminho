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
    private Integer owner;
    private String ownerType;
    
    public Contact() {
        super();
    }
    
    public Contact(String ct, String cv) {
        super(-1);
        type = ct;
        value = cv;
        owner = -1;
    }
    
    public Contact(String ct, String cv, Integer ownerId, String ownerType) {
        super(-1);
        type = ct;
        value = cv;
        owner = ownerId;
        this.ownerType = ownerType;
    }

    public Contact(Contact c){
        super(c.getId());
        type = c.getType();
        value = c.getValue();
        owner = c.getOwner();
        ownerType = c.getOwnerType();
    }
    
    public String getType() {
        return type;
    }

    public String getValue() {
        return value;
    }
    
    public Integer getOwner() {
        return owner;
    }
    
    public String getOwnerType() {
        return ownerType;
    }
    
    public void setOwnerType(String type) {
        this.ownerType = type;
    }
    
    public void setOwner(Integer id) {
        owner = id;
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
