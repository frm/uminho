/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import java.util.Set;

/**
 *
 * @author tiago
 */
public class Employee extends BasicModel{
    private String name;
    private String address;
    private String nib;
    private String nif;
    private Set<Contact> contacts;
    private String username;
    private String birthDate;
    private String education;
    private String nationality;
    private String citizenship;
    private String maritalStatus;
    private Float salary;

    public Employee() {
        super();
    }

    public Employee(String name, String address, String nif, String nib, Activity activity, Set<Contact> contacts, String username, String birthDate, String education, String nationality, String citizenship, String maritalStatus, Float salary) {
        super(-1);
        this.name = name;
        this.address = address;
        this.nib = nib;
        this.nif = nif;
        this.contacts = contacts;
        this.username = username;
        this.birthDate = birthDate;
        this.education = education;
        this.nationality = nationality;
        this.citizenship = citizenship;
        this.maritalStatus = maritalStatus;
        this.salary = salary;
    }
    
    public Employee(Employee e){
        super(e);
        this.username = e.getUsername();
        this.birthDate = e.getBirthDate();
        this.education = e.getEducation();
        this.nationality = e.getNationality();
        this.citizenship = e.getCitizenship();
        this.maritalStatus = e.getMaritalStatus();
        this.salary = e.getSalary();
    }

    public String getUsername() {
        return username;
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

    public Float getSalary() {
        return salary;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public String getNib() {
        return nib;
    }

    public void setNib(String nib) {
        this.nib = nib;
    }

    public String getNif() {
        return nif;
    }

    public void setNif(String nif) {
        this.nif = nif;
    }

    public Set<Contact> getContacts() {
        return contacts;
    }

    public void setContacts(Set<Contact> contacts) {
        this.contacts = contacts;
    }

    public void setUsername(String username) {
        this.username = username;
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

    public void setSalary(Float salary) {
        this.salary = salary;
    }

    
    @Override
    public Employee clone(){
        return new Employee(this);
    }
    
    @Override
    public String toString(){
        return name;
    }
    
    public boolean equals(Object o) {
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Employee e = (Employee) o;
        
        return (super.equals(o)
                && this.username.equals(e.getUsername())
                && this.birthDate.equals(e.getBirthDate())
                && this.education.equals(e.getEducation())
                && this.nationality.equals(e.getNationality())
                && this.citizenship.equals(e.getCitizenship())
                && this.maritalStatus.equals(e.getMaritalStatus())
                && this.salary == e.getSalary());
    }
}
