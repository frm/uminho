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
public class Representative extends Member{
    private String nib;
    private String nif;
    private String maritalStatus;
    private String education;
    private String nationality;
    private String birthPlace;
    private Activity activity;
    private Collection<Contact> contacts;
    
    public Representative(){
        super();
    }

    public Representative(String name, String birthDate, String nib, String nif, String maritalStatus, String education, String nationality, String birthPlace, Activity activity, Collection<Contact> contacts) {
        super(name, birthDate);
        this.nib = nib;
        this.nif = nif;
        this.maritalStatus = maritalStatus;
        this.education = education;
        this.nationality = nationality;
        this.birthPlace = birthPlace;
        this.activity = activity;
        this.contacts = new ArrayList<Contact>(contacts);
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

    public Activity getActivity() {
        return activity;
    }
    
    public Integer getActivityId() {
        return activity.getId();
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

    public void setActivity(Activity activity) {
        this.activity = activity;
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

    public Collection<Contact> getContacts() {
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

    public void setContacts(Collection<Contact> contacts) {
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
        
        return ( super.equals(obj) );
    }
 }