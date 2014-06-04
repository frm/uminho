
import java.util.ArrayList;
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
    
    private void addAdmin() {
        UserController uc = this.app.getUserController();
        uc.registerAdmin("Admin", "admin", "admin@fitnessum.com");
        this.app.setUserController(uc);
    }
    
    private void addTestUsers() {
        UserController uc = this.app.getUserController();
        Seed.frm(uc);
        Seed.td(uc);
        Seed.jrod(uc);
        uc.loginUser("f@r.mendes", "password1");
        uc.sendFriendRequest(2);
        uc.sendFriendRequest(3);
        uc.loginUser("td@gmail.com", "marmitas");
        uc.sendFriendRequest(3);
        uc.acceptFriendRequest(1);
        uc.loginUser("rod@m.com", "leite");
        uc.acceptFriendRequest(1);
        this.app.setUserController(uc);
    }
    
    private static void frm(UserController uc) {
        UserInfo ui = new UserInfo(true, 178.0, 64.0, new GregorianCalendar(1994, 7, 3), "Couching");
        uc.registerUser("Fernando Mendes", "f@r.mendes", "password1", ui);
    }
    
    private static void td(UserController uc) {
        UserInfo ui = new UserInfo(true, 174.0, 60.0, new GregorianCalendar(1994, 3, 26), "Kendo");
        uc.registerUser("Tiago Dinis", "td@gmail.com", "marmitas", ui);
    }
    
    private static void jrod(UserController uc) {
        UserInfo ui = new UserInfo(true, 174.0, 63.0, new GregorianCalendar(1994, 10, 13), "Swimming");
        uc.registerUser("João Rodrigues", "rod@m.com", "leite", ui);
    }
    
    public FitnessUM generate() {
        addTestUsers();
        addAdmin();
        return this.app;
    }
    
}
