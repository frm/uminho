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
    
    public Member(){}

    public Member(String name, String birthDate) {
        this.name = name;
        this.birthDate = birthDate;
    }
    
    public String getName() {
        return name;
    }

    public String getBirthDate() {
        return birthDate;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setBirthDate(String birthDate) {
        this.birthDate = birthDate;
    }
}


