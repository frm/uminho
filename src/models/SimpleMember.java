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
    private String grauParentesco;
    
    public SimpleMember(){
        super();
    }

    public SimpleMember(String grauParentesco, String name, String birthDate) {
        super(name, birthDate);
        this.grauParentesco = grauParentesco;
    }

    public SimpleMember(String name, String birthDate) {
        super(name, birthDate);
    }

    public void setGrauParentesco(String grauParentesco) {
        this.grauParentesco = grauParentesco;
    }

    public String getGrauParentesco() {
        return grauParentesco;
    }
    
    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        
        if(obj == null || this.getClass() != obj.getClass())
            return false;
        
        SimpleMember sm = (SimpleMember) obj;
        
        return ( super.equals(obj) );
    }
}
