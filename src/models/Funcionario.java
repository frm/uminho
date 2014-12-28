/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import java.util.GregorianCalendar;
import java.util.Objects;
import java.util.TreeSet;

/**
 *
 * @author tiago
 */
public class Funcionario extends BaseEntity{
    private int id;
    private GregorianCalendar dateOfBirth;
    private String education;
    private String nacionality;
    private String citizenship;
    private String maritalStatus;
    private float salary;

    public Funcionario() {
        super();
        this.id = -1;
        this.dateOfBirth = new GregorianCalendar();
        this.education = "Nothing here....";
        this.nacionality = "Nothing here....";
        this.citizenship = "Nothing here....";
        this.maritalStatus = "Nothing here....";
        this.salary = -1;
    }

    public Funcionario(String name, String adress, String nif, String nib, TreeSet<Integer> contacts, GregorianCalendar dateOfBirth, String education, String nacionality, String citizenship, String maritalStatus, float salary) {
        super(name, adress, nif, nib, contacts);
        this.id = -1;
        this.dateOfBirth = dateOfBirth;
        this.education = education;
        this.nacionality = nacionality;
        this.citizenship = citizenship;
        this.maritalStatus = maritalStatus;
        this.salary = salary;
    }
    
    public Funcionario(Funcionario f){
        super(f);
        this.id = -1;
        this.dateOfBirth = dateOfBirth;
        this.education = education;
        this.nacionality = nacionality;
        this.citizenship = citizenship;
        this.maritalStatus = maritalStatus;
        this.salary = salary;
    }

    public int getId() {
        return id;
    }

    public GregorianCalendar getDateOfBirth() {
        return dateOfBirth;
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

    public void setId(int id) {
        this.id = id;
    }

    public void setDateOfBirth(GregorianCalendar dateOfBirth) {
        this.dateOfBirth = dateOfBirth;
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
    
    @Override
    public Funcionario clone(){
        return new Funcionario(this);
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("\nID: ");
        sb.append(id);
        sb.append("\nData Nascimento: ");
        sb.append(dateOfBirth);
        sb.append("\nEscolaridade: ");
        sb.append(education);
        sb.append("\nNacionalidade: ");
        sb.append(nacionality);
        sb.append("\nNaturalidade: ");
        sb.append(citizenship);
        sb.append("\nEstado Civil: ");
        sb.append(maritalStatus);
        sb.append("\nSalário: ");
        sb.append(salary);
        return super.toString() + sb.toString();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final Funcionario other = (Funcionario) obj;
        if (this.id != other.id) {
            return false;
        }
        if (!Objects.equals(this.dateOfBirth, other.dateOfBirth)) {
            return false;
        }
        if (!Objects.equals(this.education, other.education)) {
            return false;
        }
        if (!Objects.equals(this.nacionality, other.nacionality)) {
            return false;
        }
        if (!Objects.equals(this.citizenship, other.citizenship)) {
            return false;
        }
        if (!Objects.equals(this.maritalStatus, other.maritalStatus)) {
            return false;
        }
        if (Float.floatToIntBits(this.salary) != Float.floatToIntBits(other.salary)) {
            return false;
        }
        return true;
    }
    
    
    
    
}
