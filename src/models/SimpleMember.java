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
    private int id;
    private String grauParentesco;

    public SimpleMember(String grauParentesco, String name, GregorianCalendar birthDate) {
        super(name, birthDate);
        this.id = -1;
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

    public int getId() {
        return id;
    }
    
    public void setId(int id) {
        this.id = id;
    }
    
    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        
        if(obj == null || this.getClass() != obj.getClass())
            return false;
        
        SimpleMember sm = (SimpleMember) obj;
        
        return (sm.getId() == id );
    }
}
