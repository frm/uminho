/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import java.util.Date;
import java.util.GregorianCalendar;

/**
 *
 * @author joaorodrigues
 */
public abstract class Member extends BasicModel {
    private String name;
    private GregorianCalendar birthDate;
    
    public Member(){}

    public Member(String name, GregorianCalendar birthDate) {
        this.name = name;
        this.birthDate = birthDate;
    }
    
    public String getName() {
        return name;
    }

    public GregorianCalendar getBirthDate() {
        return birthDate;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setBirthDate(GregorianCalendar birthDate) {
        this.birthDate = birthDate;
    }
    
    public void setBirthDate(Date birthDate) {
        this.birthDate.setTimeInMillis(birthDate.getTime());
    }
}


