/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashSet;
import java.util.Objects;

/**
 *
 * @author tiago
 */
public class Employee extends BaseEntity{
    private int id;
    private GregorianCalendar birthDate;
    private String education;
    private String nacionality;
    private String citizenship;
    private String maritalStatus;
    private float salary;
    private HashSet<Team> teams;

    public Employee() {
        super();
        this.id = -1;
        this.birthDate = new GregorianCalendar();
        this.education = "Nothing here....";
        this.nacionality = "Nothing here....";
        this.citizenship = "Nothing here....";
        this.maritalStatus = "Nothing here....";
        this.salary = -1;
        this.teams = new HashSet();
    }

    public Employee(String name, String adress, String nif, String nib, String activity, HashSet<Contact> contacts, GregorianCalendar birthDate, String education, String nacionality, String citizenship, String maritalStatus, float salary, HashSet<Team> teams) {
        super(name, adress, nif, nib, activity, contacts);
        this.id = -1;
        this.birthDate = birthDate;
        this.education = education;
        this.nacionality = nacionality;
        this.citizenship = citizenship;
        this.maritalStatus = maritalStatus;
        this.salary = salary;
        this.teams = new HashSet(teams);
    }
    
    public Employee(Employee f){
        super(f);
        this.id = -1;
        this.birthDate = f.getBirthDate();
        this.education = f.getEducation();
        this.nacionality = f.getNacionality();
        this.citizenship = f.getCitizenship();
        this.maritalStatus = f.getMaritalStatus();
        this.salary = f.getSalary();
        this.teams = f.getTeams();
    }

    public int getId() {
        return id;
    }
    
    public void setId(int id) {
        this.id = id;
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

    public float getSalary() {
        return salary;
    }

    public HashSet<Team> getTeams() {
        return new HashSet(teams);
    }

   public void setBirthDate(GregorianCalendar birthDate) {
        this.birthDate = birthDate;
    }
    
    public void setBirthDate(Date birthDate) {
        this.birthDate.setTimeInMillis(birthDate.getTime());
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

    public void setSalary(float salary) {
        this.salary = salary;
    }

    public void setTeams(HashSet<Team> teams) {
        this.teams = new HashSet(teams);
    }
    
    
    
    @Override
    public Employee clone(){
        return new Employee(this);
    }

    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("\n");
        sb.append(id);
        sb.append(", ");
        sb.append(birthDate);
        sb.append(", ");
        sb.append(education);
        sb.append(", ");
        sb.append(nacionality);
        sb.append(", ");
        sb.append(citizenship);
        sb.append(", ");
        sb.append(maritalStatus);
        sb.append(", ");
        sb.append(salary);
        return super.toString() + sb.toString();
    }

    @Override
    public boolean equals(Object o) {
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Employee f = (Employee) o;
        
        return (super.equals(o) && this.birthDate.equals(f.getBirthDate()) && this.education.equals(f.getEducation()) && this.nacionality.equals(f.getNacionality()) && this.citizenship.equals(f.getCitizenship()) && this.maritalStatus.equals(f.getMaritalStatus()) && this.salary == f.getSalary());
    }
    
    
    
    
}
