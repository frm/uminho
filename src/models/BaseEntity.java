/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import models.Contact;
import java.util.TreeSet;

/**
 *
 * @author tiago
 */
public abstract class BaseEntity {
    private String name;
    private String address;
    private String nif;
    private String nib;
    private String activity;
    private TreeSet<Integer> contacts;

    public BaseEntity() {
        this.name = "Nothing here...";
        this.address = "Nothing here...";
        this.nif = "Nothing here...";
        this.nib = "Nothing here...";
        this.activity = "Nothing here...";
        this.contacts = contacts;
    }
    
    public BaseEntity(String name, String adress, String nif, String nib, String activity, TreeSet<Integer> contacts) {
        this.name = name;
        this.address = adress;
        this.nif = nif;
        this.nib = nib;
        this.activity = activity;
        this.contacts = new TreeSet(contacts);
    }
    
    public BaseEntity(BaseEntity be) {
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
    
    public TreeSet<Integer> getContacts() {
        return new TreeSet(contacts);
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

    public void setContacts(TreeSet<Contact> contacts) {
        this.contacts = new TreeSet(contacts);
    }
    
    //methods
    public void addContact(int cId){
        this.contacts.add(cId);
    }
    
    public void removeContact(int cId){
        if( contacts.contains(cId) )
            contacts.remove(cId);      
    }
    
    public void editContact(int cId, String ctype, String cvalue){
        //TODO
    }
    
    public abstract BaseEntity clone();
    
    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("\nNome: ");
        sb.append(name);
        sb.append("\nMorada: ");
        sb.append(address);
        sb.append("\nNIF: ");
        sb.append(nif);
        sb.append("\nNIB: ");
        sb.append(nib);
        sb.append("\nAtividade Profissional: ");
        sb.append(activity);
        return sb.toString();
    }
    
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        BaseEntity be = (BaseEntity) o;
        
        return (this.name.equals(be.getName()) && this.address.equals(be.getAddress()) && this.nif.equals(be.getNif()) && this.nib.equals(be.getNib()) && this.activity.equals(be.getActivity()));
    }
}
