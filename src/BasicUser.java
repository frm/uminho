/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author frmendes
 */
public abstract class BasicUser {
    private String name;
    private String password;
    private String email;

    public BasicUser() {
        this.name = "";
        this.password = "";
        this.email = "";
    }

    public BasicUser(String name, String password, String email) {
        this.name = name;
        this.password = password;
        this.email = email;
    }
    
    public BasicUser(BasicUser b) {
        this.name = b.getName();
        this.email = b.getEmail();
        this.password = b.getPassword();
    }
    
    public String getEmail() {
        return email;
    }

    public String getName() {
        return name;
    }

    /* Although it doesn't make sense to implement a getPassword method for security reasons
     * it is implemented as private for copy constructor (see public User (User u) )
     */
    private String getPassword() {
        return password;
    }
    
    public void setEmail(String email) {
        this.email = email;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setPassword(String password) {
        this.password = password;
    }
    
    /** For a given password, sees if it matches the users
    @param password given password
    @return true or false
    */
    public boolean matchPassword(String password) {
        return this.password.equals(password);
    }
    
    @Override
   public String toString() {
        StringBuilder result = new StringBuilder();
        result.append("\nName: ");
        result.append(this.name);
        result.append("\nE-Mail: ");
        result.append(this.email);
        
        return result.toString();
   }

   @Override
    public boolean equals(Object o) {
        if(this == o) return true;

        if(o == null || this.getClass() != o.getClass() ) return false;
        
        BasicUser b = (BasicUser) o;
        
        return (
                this.name.equals( b.getName() ) &&
                this.password.equals( b.getPassword() ) &&
                this.email.equals( b.getEmail() )
                );
     }
   
}