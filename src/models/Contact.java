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
    private int id;
    private String type;
    private String value;
    
    public Contact(){
        id = -1;
        type = "Nothing here...";
        value = "Nothing here...";
    }
    
    public Contact(String ct, String cv){
        id = -1;
        type = ct;
        value = cv;
    }

    public Contact(Contact c){
        id = c.getId();
        type = c.getType();
        value = c.getValue();
    }

    public int getId() {
        return id;
    }
    
    public String getType() {
        return type;
    }

    public String getValue() {
        return value;
    }

    public void setId(int id) {
        this.id = id;
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
    
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("\n");
        sb.append(id);
        sb.append(", ");
        sb.append(type);
        sb.append(", ");
        sb.append(value);
        return sb.toString();
    }
    
    public boolean equals(Object o) {
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Contact c = (Contact) o;
        
        return (this.type == c.getType() && this.value == c.getValue());
    }
}
