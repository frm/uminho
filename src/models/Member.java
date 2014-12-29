/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import java.util.GregorianCalendar;

/**
 *
 * @author joaorodrigues
 */
public abstract class Member {
    private String name;
    private GregorianCalendar birthDate;

    public Member(String name, GregorianCalendar birthDate) {
        this.name = name;
        this.birthDate = birthDate;
    }

    public Member() {
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
    
}


