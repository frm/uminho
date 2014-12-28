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
public class SimpleMember extends Member{
    String grauParentesco;

    public SimpleMember(String grauParentesco, String name, GregorianCalendar birthDate) {
        super(name, birthDate);
        this.grauParentesco = grauParentesco;
    }

    public SimpleMember(String grauParentesco) {
        this.grauParentesco = grauParentesco;
    }

    public SimpleMember(String name, GregorianCalendar birthDate) {
        super(name, birthDate);
    }

    public void setGrauParentesco(String grauParentesco) {
        this.grauParentesco = grauParentesco;
    }

    public String getGrauParentesco() {
        return grauParentesco;
    }
    
    
    
}
