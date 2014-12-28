/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import java.util.ArrayList;
import java.util.Collection;

/**
 *
 * @author joaorodrigues
 */
class Representative extends Member{
    private int id;
    private String nib;
    private String nif;
    private String maritalStatus;
    private String education;
    private String nationality;
    private String activity;
    private Collection<Integer> contacts;

    public Representative(String nib, String nif, String maritalStatus, String education, String nationality,String activity, Collection<Integer> contacts) {
        this.id = -1;
        this.nib = nib;
        this.nif = nif;
        this.maritalStatus = maritalStatus;
        this.education = education;
        this.nationality = nationality;
        this.activity = activity;
        this.contacts = new ArrayList<Integer>(contacts);
    }
    
    
    
    
    
    public int getId() {
        return id;
    }

    public String getNib() {
        return nib;
    }

    public String getNif() {
        return nif;
    }

    public String getMaritalStatus() {
        return maritalStatus;
    }

    public String getEscolaridade() {
        return education;
    }

    public String getNacionalidade() {
        return nationality;
    }

    public Collection<Integer> getContacts() {
        return new ArrayList<Integer>(contacts);
    }

    public void setNib(String nib) {
        this.nib = nib;
    }

    public void setNif(String nif) {
        this.nif = nif;
    }

    public void setMaritalStatus(String maritalStatus) {
        this.maritalStatus = maritalStatus;
    }

    public void setEscolaridade(String education) {
        this.education = education;
    }

    public void setNacionalidade(String nationality) {
        this.nationality = nationality;
    }

    public void setContacts(Collection<Integer> contacts) {
        this.contacts = new ArrayList<Integer>(contacts);
    }
     
 }