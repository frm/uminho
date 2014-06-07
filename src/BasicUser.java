
import java.io.Serializable;

/**
 *
 * @author frmendes
 */
public abstract class BasicUser implements Serializable{
    private String name;
    private String password;
    private String email;

    /**
     *
     */
    public BasicUser() {
        this.name = "";
        this.password = "";
        this.email = "";
    }

    /**
     *
     * @param name
     * @param password
     * @param email
     */
    public BasicUser(String name, String password, String email) {
        this.name = name;
        this.password = password;
        this.email = email;
    }
    
    /**
     *
     * @param b
     */
    public BasicUser(BasicUser b) {
        this.name = b.getName();
        this.email = b.getEmail();
        this.password = b.getPassword();
    }
    
    /**
     *
     * @return
     */
    public String getEmail() {
        return email;
    }

    /**
     *
     * @return
     */
    public String getName() {
        return name;
    }

    @Override
    public int hashCode() {
        return this.name.hashCode();
    }

    
    /* Although it doesn't make sense to implement a getPassword method for security reasons
     * it is implemented as private for copy constructor (see public User (User u) )
     */
    private String getPassword() {
        return password;
    }
    
    /**
     *
     * @param email
     */
    public void setEmail(String email) {
        this.email = email;
    }

    /**
     *
     * @param name
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     *
     * @param password
     */
    public void setPassword(String password) {
        this.password = password;
    }
    
    /**
     *
     * @param pw
     */
    public void updatePassword(String pw) {
        if (pw.length() != 0)
            this.password = pw;
    }
    
    /** For a given password, sees if it matches the users
    @param password given password
    @return true or false
    */
    public boolean matchPassword(String password) {
        return this.password.equals(password);
    }
    
    public abstract BasicUser clone();
    
    @Override
   public String toString() {
        StringBuilder result = new StringBuilder();
        result.append("\nName: ");
        result.append(this.name);
        result.append("\tE-Mail: ");
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