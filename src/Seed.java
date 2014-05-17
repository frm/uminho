
import java.util.GregorianCalendar;

/**
 *
 * @author frmendes
 */
public class Seed {
    
    private FitnessUM app;
    
    public Seed() {
        this.app = new FitnessUM();
    }
    
    public Seed(FitnessUM app) {
        this.app = app.clone();
    }
    
    private void generateAdmin() {
        UserController uc = this.app.getUserController();
        // add admin
        this.app.setUserController(uc);
    }
    
    private void generateDefaultActivityList() {
    }
    
    private void addTestUsers() {
        UserController uc = this.app.getUserController();
        Seed.frm(uc);
        Seed.td(uc);
        Seed.jrod(uc);
        uc.loginUser("f@r.away", "frmrules");
        uc.addFriend(2);
        uc.addFriend(3);
        uc.loginUser("td@gmail.com", "marmitas");
        uc.addFriend(3);
        this.app.setUserController(uc);
    }
    
    private static void frm(UserController uc) {
        UserInfo ui = new UserInfo(true, 178.0, 64.0, new GregorianCalendar(1994, 8, 3), "Couching");
        uc.registerUser("Fernando Mendes", "f@r.away", "frmrules", ui);
    }
    
    private static void td(UserController uc) {
        UserInfo ui = new UserInfo(true, 174.0, 60.0, new GregorianCalendar(1994, 4, 26), "Kendo");
        uc.registerUser("Tiago Dinis", "td@gmail.com", "marmitas", ui);
    }
    
    private static void jrod(UserController uc) {
        UserInfo ui = new UserInfo(true, 174.0, 60.0, new GregorianCalendar(1994, 4, 26), "Kendo");
        uc.registerUser("João Rodrigues", "bobloblaw@law.blog", "lowblow", ui);
    }
    
    public FitnessUM generate() {
        this.addTestUsers();
        return this.app;
    }
    
}
