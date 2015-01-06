/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

/**
 *
 * @author joaorodrigues
 */
public abstract class Member extends BasicModel {
    private String name;
    private String birthDate;
    private Integer familyID;
    
    public Member(){}

    public Member(String name, String birthDate, Integer familyID) {
        super(-1);
        this.name = name;
        this.birthDate = birthDate;
        this.familyID = familyID;
    }
    
    public String getName() {
        return name;
    }

    public String getBirthDate() {
        return birthDate;
    }

    public Integer getFamilyID() {
        return familyID;
    }

    public void setFamilyID(Integer familyID) {
        this.familyID = familyID;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setBirthDate(String birthDate) {
        this.birthDate = birthDate;
    }
}


