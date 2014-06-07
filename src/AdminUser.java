
/**Admin user, can control the system
 *
 * @author frmendes
 */
public class AdminUser extends BasicUser{

    /**
     *
     */
    public AdminUser() {
        super();
    }
    
    /**
     *
     * @param name
     * @param password
     * @param email
     */
    public AdminUser( String name, String password, String email ) {
        super(name, password, email);
    }
    
    /**
     *
     * @param a
     */
    public AdminUser( AdminUser a ) {
        super(a);
    }
    
    @Override
    public String toString() {
        return "### Admin: ###" + super.toString();
    }
    
    @Override
    public AdminUser clone() {
        return new AdminUser(this);
    }
    
}