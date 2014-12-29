/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import java.util.GregorianCalendar;
import java.util.TreeSet;

/**
 *
 * @author tiago
 */
public class Volunteer extends BaseEntity{
    private int id;
    private GregorianCalendar birthDate;
    private String education;
    private String nacionality;
    private String citizenship;
    private String maritalStatus;
    private String observations;
    private String file;

    public Volunteer() {
        super();
        this.id = -1;
        this.birthDate = new GregorianCalendar();
        this.education = "Nothing here...";
        this.nacionality = "Nothing here...";
        this.citizenship = "Nothing here...";
        this.maritalStatus = "Nothing here...";
        this.observations = "Nothing here...";
        this.file = "Nothing here...";
    }

    public Volunteer(String name, String adress, String nif, String nib, String activity, TreeSet<Integer> contacts, GregorianCalendar birthDate, String education, String nacionality, String citizenship, String maritalStatus, String observations, String file) {
        super(name, adress, nif, nib, activity, contacts);
        this.birthDate = birthDate;
        this.education = education;
        this.nacionality = nacionality;
        this.citizenship = citizenship;
        this.maritalStatus = maritalStatus;
        this.observations = observations;
        this.file = file;
    }
    
    public Volunteer(Volunteer v){
        super(v);
        this.birthDate = v.getBirthDate();
        this.education = v.getEducation();
        this.nacionality = v.getNacionality();
        this.citizenship = v.getCitizenship();
        this.maritalStatus = v.getMaritalStatus();
        this.observations = v.getObservations();
        this.file = v.getFile();
    }

    public GregorianCalendar getBirthDate() {
        return birthDate;
    }

    public String getEducation() {
        return education;
    }

    public String getNacionality() {
        return nacionality;
    }

    public String getCitizenship() {
        return citizenship;
    }

    public String getMaritalStatus() {
        return maritalStatus;
    }

    public String getObservations() {
        return observations;
    }

    public String getFile() {
        return file;
    }

    public void setBirthDate(GregorianCalendar birthDate) {
        this.birthDate = birthDate;
    }

    public void setEducation(String education) {
        this.education = education;
    }

    public void setNacionality(String nacionality) {
        this.nacionality = nacionality;
    }

    public void setCitizenship(String citizenship) {
        this.citizenship = citizenship;
    }

    public void setMaritalStatus(String maritalStatus) {
        this.maritalStatus = maritalStatus;
    }

    public void setObservations(String observations) {
        this.observations = observations;
    }

    public void setFile(String file) {
        this.file = file;
    }
    
    @Override
    public Volunteer clone(){
        return new Volunteer(this);
    }
    
    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("\nID: ");
        sb.append(id);
        sb.append("\nData Nascimento: ");
        sb.append(birthDate);
        sb.append("\nEscolaridade: ");
        sb.append(education);
        sb.append("\nNacionalidade: ");
        sb.append(nacionality);
        sb.append("\nNaturalidade: ");
        sb.append(citizenship);
        sb.append("\nEstado Civil: ");
        sb.append(maritalStatus);
        sb.append("\nObservações: ");
        sb.append(observations);
        sb.append("\nFicheiro: ");
        sb.append(file);
        return super.toString() + sb.toString();
    }
    
    public boolean equals(Object o) {
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Volunteer v = (Volunteer) o;
        
        return (super.equals(o) && this.birthDate.equals(v.getBirthDate()) && this.education.equals(v.getEducation()) && this.nacionality.equals(v.getNacionality()) && this.citizenship.equals(v.getCitizenship()) && this.maritalStatus.equals(v.getMaritalStatus()) && this.observations.equals(v.getObservations()) && this.file.equals(v.getFile()));
    }
}
