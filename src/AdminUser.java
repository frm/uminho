/**
 *
 * @author frmendes
 */
public class AdminUser extends BasicUser {

    public AdminUser() {
        super();
    }
    
    public AdminUser( String name, String password, String email ) {
        super(name, password, email);
    }
    
    public AdminUser( AdminUser a ) {
        super(a);
    }
    
    public String toString() {
        return "### Admin: ###" + super.toString();
    }
    
    public AdminUser clone() {
        return new AdminUser(this);
    }
    
}