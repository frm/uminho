/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import java.util.HashSet;
import java.util.Set;

/**
 *
 * @author tiago
 */
public class Volunteer extends BaseEntity {
    private String birthDate;
    private String education;
    private String nationality;
    private String citizenship;
    private String maritalStatus;
    private String observations;
    private String file;
    private Integer currentTeam;
    private Integer donorId;
    private Set<Integer> teams;

    public Volunteer() {
        super();
    }

    public Volunteer(String name, String address, String nif, String nib, Activity activity, Set<Contact> contacts, String birthDate, String education, String nationality, String citizenship, String maritalStatus, String observations, String file, Integer donorId) {
        super(name, address, nif, nib, activity, contacts);
        this.birthDate = birthDate;
        this.education = education;
        this.nationality = nationality;
        this.citizenship = citizenship;
        this.maritalStatus = maritalStatus;
        this.observations = observations;
        this.file = file;
        this.currentTeam = 1;
        this.teams = new HashSet<>();
    }
    
    public Volunteer(Volunteer v){
        super(v);
        this.birthDate = v.getBirthDate();
        this.education = v.getEducation();
        this.nationality = v.getNationality();
        this.citizenship = v.getCitizenship();
        this.maritalStatus = v.getMaritalStatus();
        this.observations = v.getObservations();
        this.file = v.getFile();
        this.donorId = v.getDonorId();
    }

    public Integer getDonorId() {
        return donorId;
    }

    public void setDonorId(Integer donorId) {
        this.donorId = donorId;
    }

    
    public String getBirthDate() {
        return birthDate;
    }

    public String getEducation() {
        return education;
    }

    public String getNationality() {
        return nationality;
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
    
    public void setBirthDate(String birthDate) {
        this.birthDate = birthDate;
    }
    
    public void setEducation(String education) {
        this.education = education;
    }

    public void setNationality(String nationality) {
        this.nationality = nationality;
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
    
    public void setCurrentTeam(Integer i) {
        this.currentTeam = i;
    }
    
    public Integer getCurrentTeam() {
        return this.currentTeam;
    }
    
    public Set<Integer> getTeams() {
        return this.teams;
    }
    
    public void addTeam(Integer i) {
        this.teams.add(i);
    }
    
    @Override
    public Volunteer clone(){
        return new Volunteer(this);
    }
    
    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder(super.toString());
        sb.append(", ");
        sb.append(birthDate);
        sb.append(", ");
        sb.append(education);
        sb.append(", ");
        sb.append(nationality);
        sb.append(", ");
        sb.append(citizenship);
        sb.append(", ");
        sb.append(maritalStatus);
        sb.append(", ");
        sb.append(observations);
        sb.append(", ");
        sb.append(file);
        return sb.toString();
    }
    
    public boolean equals(Object o) {
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Volunteer v = (Volunteer) o;
        
        return (super.equals(o)
                && this.birthDate.equals(v.getBirthDate())
                && this.education.equals(v.getEducation())
                && this.nationality.equals(v.getNationality())
                && this.citizenship.equals(v.getCitizenship())
                && this.maritalStatus.equals(v.getMaritalStatus())
                && this.observations.equals(v.getObservations())
                && this.file.equals(v.getFile()));
    }
}
