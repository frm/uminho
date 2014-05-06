/**
 *
 * @author frmendes
 */

public class User {
    private String name;
    private String password;
    private String email;
    private int id;
    
    public User() {
        this.name = "";
        this.password = "";
        this.email = "";
        this.id = -1;
    }

    public User(String name, String password, String email) {
        this.name = name;
        this.password = password;
        this.email = email;
        this.id = -1;
    }
    
    public User(User u) {
        this.id = u.getId();
        this.name = u.getName();
        this.email = u.getEmail();
        this.password = u.getPassword();        
    }

    public String getEmail() {
        return email;
    }
    
/* Although it doesn't make sense to implement a getPassword method for security reasons
 * it is implemented as private for copy constructor (see public User (User u) )
 */
    private String getPassword() {
        return password;
    }

    public int getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public void setId(int id) {
        this.id = id;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public User clone() {
        return new User(this);
    }
    
    public String toString() {
        StringBuilder result = new StringBuilder();
        result.append("Utilizador: ");
        result.append("\nE-Mail: ");
        result.append(this.email);
        result.append("\nName: ");
        result.append(this.name);

        return result.toString();
    }

    
    public boolean equals(Object o) {
        if(this == o) return true;

        if(o == null || this.getClass() != o.getClass() ) return false;

        User u = (User) o;
        
       return (u.getEmail() == this.email && u.getPassword() == this.password && u.getName() == this.name );
    }

    public int hashCode() {
        return this.getId();
    }
}
