/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import java.util.HashSet;

/**
 *
 * @author tiago
 */
public abstract class BaseEntity extends BasicModel {
    private String name;
    private String address;
    private String nif;
    private String nib;
    private String activity;
    private HashSet<Contact> contacts;

    public BaseEntity() {}
    
    public BaseEntity(String name, String address, String nif, String nib, String activity, HashSet<Contact> contacts) {
        super(-1);
        this.name = name;
        this.address = address;
        this.nif = nif;
        this.nib = nib;
        this.activity = activity;
        this.contacts = new HashSet(contacts);
    }
    
    public BaseEntity(BaseEntity be) {
        super(be.getId());
        this.name = be.getName();
        this.address = be.getAddress();
        this.nif = be.getNif();
        this.nib = be.getNib();
        this.activity = be.getActivity();
        this.contacts = be.getContacts();
    }
    
    //getters
    public String getName() {
        return name;
    }

    public String getAddress() {
        return address;
    }

    public String getNif() {
        return nif;
    }

    public String getNib() {
        return nib;
    }

    public String getActivity() {
        return activity;
    }
    
    public HashSet<Contact> getContacts() {
        return new HashSet(contacts);
    }

    //setters
    public void setName(String name) {
        this.name = name;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public void setNif(String nif) {
        this.nif = nif;
    }

    public void setNib(String nib) {
        this.nib = nib;
    }
    
    public void setActivity(String activity){
        this.activity = activity;
    }
    public void setContacts(HashSet<Contact> contacts) {
        this.contacts = new HashSet(contacts);
    }
    
    //methods
    
    public void removeContact(int cId){
        if( contacts.contains(cId) )
            contacts.remove(cId);      
    }
    
    public void editContact(int cId, String ctype, String cvalue){
        //TODO
    }
    
    @Override
    public abstract BaseEntity clone();
    
    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder(super.toString());
        sb.append(", ");
        sb.append(name);
        sb.append(", ");
        sb.append(address);
        sb.append(", ");
        sb.append(nif);
        sb.append(", ");
        sb.append(nib);
        sb.append(", ");
        sb.append(activity);
        return sb.toString();
    }
    
    @Override
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        BaseEntity be = (BaseEntity) o;
        
        return (super.equals(o) && this.name.equals(be.getName()) && this.address.equals(be.getAddress()) && this.nif.equals(be.getNif()) && this.nib.equals(be.getNib()) && this.activity.equals(be.getActivity()));
    }
}
