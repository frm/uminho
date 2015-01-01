/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import java.util.ArrayList;
import java.util.Collection;
import java.util.GregorianCalendar;

/**
 *
 * @author joaorodrigues
 */
class Representative extends Member{
    private Integer id;
    private String nib;
    private String nif;
    private String maritalStatus;
    private String education;
    private String nationality;
    private String birthPlace;
    private String activity;
    private Collection<Integer> contacts;
    
    public Representative(){
        super();
    }

    public Representative(String name, GregorianCalendar birthDate, String nib, String nif, String maritalStatus, String education, String nationality, String birthPlace, String activity, Collection<Integer> contacts) {
        super(name, birthDate);
        this.id = -1;
        this.nib = nib;
        this.nif = nif;
        this.maritalStatus = maritalStatus;
        this.education = education;
        this.nationality = nationality;
        this.birthPlace = birthPlace;
        this.activity = activity;
        this.contacts = new ArrayList<Integer>(contacts);
    }

    public String getEducation() {
        return education;
    }

    public String getNationality() {
        return nationality;
    }

    public String getBirthPlace() {
        return birthPlace;
    }

    public String getActivity() {
        return activity;
    }

    public void setEducation(String education) {
        this.education = education;
    }

    public void setNationality(String nationality) {
        this.nationality = nationality;
    }

    public void setBirthPlace(String birthPlace) {
        this.birthPlace = birthPlace;
    }

    public void setActivity(String activity) {
        this.activity = activity;
    }
    
    public Integer getId() {
        return id;
    }
    
    public void setId(Integer id) {
        this.id = id;
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

    public Collection<Integer> getContacts() {
        return contacts;
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

    public void setContacts(Collection<Integer> contacts) {
        this.contacts = contacts;
    }
        @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        
        if(obj == null || this.getClass() != obj.getClass())
            return false;
        
        Representative r = (Representative) obj;
        
        return (r.getId() == id );
    }
 }