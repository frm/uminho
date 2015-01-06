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
public class SimpleMember extends Member{
    private String kinship;
    
    public SimpleMember(){
        super();
    }

    public SimpleMember(String name, String birthDate, String kinship, Integer familyID) {
        super(name, birthDate, familyID);
        this.kinship = kinship;
    }

    public void setKinship(String kinship) {
        this.kinship = kinship;
    }

    public String getKinship() {
        return kinship;
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
