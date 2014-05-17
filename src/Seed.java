
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
    
    private void generateAdmin() {
        UserController uc = this.app.getUserController();
        // add admin
        this.app.setUserController(uc);
    }
    
    private void generateDefaultActivityList() {
        ArrayList<String> simple = new ArrayList<String>();
        ArrayList<String> distance = new ArrayList<String>();
        ArrayList<String> altitude = new ArrayList<String>();
        
        simple.add("Aerobics");
        simple.add("Badminton");
        simple.add("Basketball");
        simple.add("Squash");
        simple.add("Tennis");
        simple.add("Table Tennis");
        simple.add("Gymnastics");
        simple.add("Handball");
        simple.add("Hockey");
        simple.add("Martial Arts");
        simple.add("Pilates");
        simple.add("Fencing");
        simple.add("Dancing");
        simple.add("Boxing");
        simple.add("Climbing Stairs");
        simple.add("Skating");
        simple.add("Baseball");
        simple.add("Polo");
        simple.add("Cricket");
        simple.add("Football");
        simple.add("American Football");
        
        distance.add("Swimming");
        distance.add("Windsurfing");
        distance.add("Sailing");
        distance.add("Rowing");
        distance.add("Kayaking");
        distance.add("Kite Surfing");
        distance.add("Golfing");
        distance.add("Riding");
        distance.add("Indoor Cycling");
        
        altitude.add("Cycling");
        altitude.add("Mountain Biking");
        altitude.add("Running");
        altitude.add("Walking");
        altitude.add("Hiking");
        altitude.add("Orienteering");
        altitude.add("Roller Skiing");
        altitude.add("Skiing");
        altitude.add("Snowboarding");
        altitude.add("Elliptical Training");
        altitude.add("Scuba Diving");
        
        app.setActivities(simple, distance, altitude);
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
