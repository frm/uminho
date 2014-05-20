
import java.util.ArrayList;
import java.util.GregorianCalendar;

/**
 *
 * @author frmendes
 */

public class FitnessUM {

    private boolean active;
    private UserController userController;
    private ActivityController activityController;


   private static final String[] startOptions = { "Exit", "Register", "Login" };

   private static final String[] mainOptions = {
       "Logout", "My Profile", "Friend List","My Activity Log", "Add New Activity Session", "Show My Statistics"

   };

   private static final String[] addActivitySessionOptions = {
       "Go Back", "Simple Activities", "Distance Activities", "Altitude Activities"
   };

   private static final String[] statsOptions = {
       "Go Back", "Check all the statistics", "Check statistics for one activity"
   };



    /** Empty constructor
     */
    public FitnessUM() {
        this.userController = new UserController();
        this.activityController = new ActivityController();
    }

    /** Parameterized constructor
     * @param users existing UserDatabase
     */
    public FitnessUM(UserController userController, ActivityController activityController) {
        this.userController = userController.clone();
        this.activityController = activityController.clone();
    }

    /** Copy constructor
     * @param fit existing FitnessUM app
     */
    public FitnessUM(FitnessUM fit) {
        this.userController = fit.getUserController();
        this.activityController = fit.getActivityController();
    }

    /** Getter for logged in user ID
     * @return logged in user ID
     */
    public User getUserCurrentUser() {
        return this.userController.getCurrentUser();
    }

    public UserController getUserController() {
        return this.userController.clone();
    }

    public ActivityController getActivityController(){
        return this.activityController.clone();
    }

    public void setUserController(UserController uc) {
        this.userController = uc.clone();
    }

     public void setActivityController(ActivityController ac) {
        this.activityController = ac.clone();
    }

    /** Getter for active variable
     * @return active variable
     */
    public boolean isActive() {
        return this.active;
    }

    /** Setter for active variable
     */
    public void startup() {
        this.active = true;
    }

    /** Setter for active variable
     */
    public void shutdown() {
        this.active = false;
    }

    /** Scans for information and saves the user into the database
      */
    public void registerUser() {
        String name = Scan.name("\nFirst name: ") + " " + Scan.name("\nLast name: ");
        String email = Scan.email();

        while ( ! this.userController.validateEmailUniqueness(email) ) {
            System.out.println("Email is already taken");
            email = Scan.email();
        }

        String password = Scan.password();
        UserInfo info = Scan.userInfo();

        this.userController.registerUser(name, email, password, info);
        this.userController.loginUser(email, password);
    }
    
    private void searchUserByName() {
        String name = Scan.name("Enter a name:");
        new SearchUserNavigator( this.userController.nameSearch(name) ).navigate();
    }
    
    private void searchUserByEmail() {
        String email = Scan.email();
        new SearchUserNavigator( this.userController.emailSearch(email) ).navigate();
    }
    
    public void searchUser() {
        final FitnessUM app = this;
        
        Prompt[] p = new Prompt[] {
            new Prompt() { public void exec() { return; } },
            new Prompt() { public void exec() { app.searchUserByName(); } },
            new Prompt() { public void exec() { app.searchUserByEmail(); } }
         };
         
        System.out.println("\n0. Go Back\n1. By Name\n2. By Email\n");
        
        int option = Scan.menuOption(0, 2);
         p[option].exec();        
    }

    /** Scans for valid login info and sets the current_user
     */
    public void loginUser() {
        int nrAttempts = 0;
        boolean logged = false;

        while (nrAttempts < 3 && !logged) {
            String email = Scan.scanString("Enter email:");

            while ( !this.userController.existsUserWithEmail(email) ) {
               System.out.println("We have no record of that email...");
               email = Scan.scanString("Enter email:");
            }

            String pw = Scan.scanString("Enter password:");

            if ( this.userController.loginUser(email, pw) )
                logged = true;
            else
                System.out.println("Password and email don't match. " + (3 - ++nrAttempts) + " attempt(s) remaining.");

        }

        if (! logged) {
            System.out.println("Too many failed attempts. We called the cops.\nBye bye.");
            this.shutdown();
        }

        System.out.println("\nWelcome "+ this.userController.getCurrentUser().getName() );
    }

    public void userProfile() {
        System.out.println( this.userController.currentUserProfile() );
        Scan.pressEnterToContinue();
    }

    public void listFriends() {
        new FriendListNavigator( this.userController.getFriendList() ).navigate();
    }
    
    public void addFriend(User u) {
        this.userController.addFriend(u);
    }

    public void getAddActivitySessionOption() {
        System.out.println("Choose one of the following options.");
        FitnessUM.printAddActivitySessionOptions();
        int option = Scan.menuOption(0, 3);
        this.getAddActivitySessionPrompt()[option].exec();
    }

    public void listAddActivitySession(){
        this.getAddActivitySessionOption();
    }
    
    public void listStats(){
        this.getStatsOption();
    }

    public void showStatsByName(String name){
        System.out.println( userController.showStats(name) );
    }
    
    public void showAllStats(){
        System.out.println( userController.showStats() );
    }
    
    public void getStatsOption(){
        System.out.println("Choose one of the following options.");
        FitnessUM.printStatsOptions();
        int option = Scan.menuOption(0, 2);
        this.getStatsPrompt()[option].exec();
    }

    public void getCategoryStatsOption(){
        System.out.println("Choose one of the following options.");
        FitnessUM.printAddActivitySessionOptions();
        int option = Scan.menuOption(0, 3);
        this.getCategoryStatsPrompt()[option].exec();
    }



    public static String listWeatherOptions(){
        String[] list = Weather.weatherStates;
        StringBuilder result = new StringBuilder();
        result.append("How was the weather?\n");
        for( String w: list){
            result.append(Weather.getIndexOf(w)).append(".").append(w).append("\n");
        }
        return result.toString() ;
    }

    public void addActivitySession(int category, String name) {

        /*Falta receber se é distance ou altitude ou nada, usar variavel specialCategories, que diz o numero de categorias extra*/
        /*Falta por o navigator para as atividades e os weathers*/
        int weather = Scan.scanInt( FitnessUM.listWeatherOptions() );
        int calories = Scan.scanInt("How many calories did you burn?");
        GregorianCalendar date = Scan.dateWithHours("What's the date of this session? (dd-mm-yyyy)", "At what time did it start? (hh:mm)");
        GregorianCalendar duration = Scan.duration("How long was the session? (hh:mm:ss)");

        if(category == 1){
            int distance = Scan.scanInt("What was the distance?");
            userController.addActivity(name, weather, date, duration, calories, distance);
        }

        else if(category == 2){
            int distance = Scan.scanInt("What was the distance?");
            int altitude = Scan.scanInt("What was the altitude?");
            userController.addActivity(name, weather, date, duration, calories, distance, altitude);
        }

        else userController.addActivity(name, weather, date, duration, calories);
    }
    
    public void myActivityLog(){
        ArrayList<Activity> list = userController.getMostRecentActivities();
        
        System.out.print(list.toString()+"\n");
    }

    /** Reads an integer from the user input and starts up or shuts down the app accordingly
     */
    public void getStartOption() {
        this.startup();
        System.out.println("Choose one of the following options.");
        FitnessUM.printStartOptions();
        int option = Scan.menuOption(0, 2);
        this.getStartPrompt()[option].exec();
    }

    /** Reads user input and launches a chain of events accordingly
     */
    public void commandInterpreter() {
        System.out.println( "Choose one of the following options.");
        FitnessUM.printMainOptions();
        int option = Scan.menuOption(0, 5);
        this.getMainPrompt()[option].exec();
    }

    /** Controls the main flow of events.
     */
    public void run() {
        System.out.println("\nWelcome to FitnessUM");

        this.getStartOption();
        while ( this.isActive() )
            this.commandInterpreter();
        
    }

    private Prompt[] getStartPrompt() {
        final FitnessUM app = this;
        return new Prompt[] {
            new Prompt() { public void exec() { app.shutdown(); } },
            new Prompt() { public void exec() { app.registerUser();} },
            new Prompt() { public void exec() { app.loginUser();} }
        };
    }

    private Prompt[] getMainPrompt() {
        final FitnessUM app = this;
        return new Prompt[] {
            new Prompt() { public void exec() { app.run(); } },
            new Prompt() { public void exec() { app.userProfile(); } },
            new Prompt() { public void exec() { app.listFriends(); }},
            new Prompt() { public void exec() { app.searchUser(); }},
            new Prompt() { public void exec() { app.myActivityLog(); }},
            new Prompt() { public void exec() { app.listAddActivitySession(); } },
            new Prompt() { public void exec() { app.listStats(); } }
        };
    }

    private Prompt[] getAddActivitySessionPrompt() {
        final FitnessUM app = this;
        return new Prompt[] {
            new Prompt() { public void exec() { return; } },
            new Prompt() { public void exec() { (new AddActivityNavigator( 0,app, app.activityController.getSimpleActivities() ) ).navigate(); } },
            new Prompt() { public void exec() { (new AddActivityNavigator( 1,app, app.activityController.getDistanceActivities() ) ).navigate();} },
            new Prompt() { public void exec() { (new AddActivityNavigator( 2,app, app.activityController.getAltitudeActivities() ) ).navigate();} }
        };
    }
    
    public Prompt[] getStatsPrompt(){
        final FitnessUM app = this;
        return new Prompt[]{
            new Prompt(){ public void exec(){ return;}},
            new Prompt() { public void exec() { app.showAllStats(); } },
            new Prompt() { public void exec() { app.getCategoryStatsPrompt(); } }
        };
    }
    
    
    public Prompt[] getCategoryStatsPrompt(){
        final FitnessUM app = this;
        return new Prompt[]{
            new Prompt(){ public void exec(){ return;}},
            new Prompt() { public void exec() { (new StatsNavigator( 0,app, app.activityController.getSimpleActivities() ) ).navigate(); } },
            new Prompt() { public void exec() { (new StatsNavigator( 1,app, app.activityController.getDistanceActivities() ) ).navigate();} },
            new Prompt() { public void exec() { (new StatsNavigator( 2,app, app.activityController.getAltitudeActivities() ) ).navigate();} }
        };
    }

    private static Prompt[] getDevPrompt() {
        return new Prompt[] {
            new Prompt() { public void exec() { System.out.println("\nBye bye."); } },
            new Prompt() { public void exec() { new FitnessUM( new Seed().generate() ).run(); } },
            new Prompt() { public void exec() { System.out.println("\nFunction yet to be implemented\n"); FitnessUM.devPrompt(); } }
        };
     }

    private static void devPrompt() {
        System.out.println("Do you wish to import an existing network or create a new one?\n0. Exit\n1. Create\n2. Import");

        int option = Scan.menuOption(0, 2);
        FitnessUM.getDevPrompt()[option].exec();
    }

    private static void printStartOptions() {
        int i = 0;
        for (String s : FitnessUM.startOptions)
            System.out.println(i++ + ". " + s);
    }

    private static void printAddActivitySessionOptions() {
        int i = 0;
        for (String s : FitnessUM.addActivitySessionOptions)
            System.out.println(i++ + ". " + s);
    }

    private static void printMainOptions() {
        int i = 0;
        for (String s : FitnessUM.mainOptions)
            System.out.println(i++ + ". " + s);
    }

    private static void printStatsOptions() {
        int i = 0;
        for (String s : FitnessUM.statsOptions)
            System.out.println(i++ + ". " + s);
    }


    public FitnessUM clone() {
        return new FitnessUM(this);
    }

    public static void main(String[] args) {
        System.out.println("Welcome to FitnessUM Dev Prompt.");
        FitnessUM.devPrompt();
    }
}
