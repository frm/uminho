
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
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
       "Logout", "My Profile", "Friend Requests", "Friend List","Search User", "My Activity Log", "Add New Activity Session", "Show My Statistics"

   };
   
    private static final String[] activityCategories = {
       "Go Back", "Simple Activities", "Distance Activities", "Altitude Activities"
   };
   
   private static final String[] statsOptions = {
       "Go Back", "Check all the statistics", "Check statistics for one activity"
   };
   
   private static final String[] activities = {
       "Go Back", "Cycling", "Kayaking", "Kendo", "Running", "Skating", "Swimming"
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
    public FitnessUM(UserController userController) {
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
        UserInfo info = userInfo();

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
        else greet();
    }

    private void greet() {
        System.out.println("\nWelcome "+ this.userController.getCurrentUser().getName() );
        if ( this.userController.hasFriendRequests() )
            System.out.println("You have friend requests!");
    }

    public void userProfile() {
        System.out.println( this.userController.currentUserProfile() );
        Scan.pressEnterToContinue();
    }

    public void listFriends() {
        new FriendListNavigator( this.userController.getFriendList() ).navigate();
    }

    public void addFriend(User u) {
        this.userController.sendFriendRequest(u);
    }

    public void deleteFriend(User u) {
        this.userController.deleteFriend(u);
    }

    public void acceptFriend(User u) {
        this.userController.acceptFriendRequest(u);
    }

    public void rejectFriend(User u) {
        this.userController.rejectFriendRequest(u);
    }
    
    private void viewFriendRequests() {
        new FriendRequestsNavigator( this.userController.getFriendRequests() ).navigate();
    }
    
    public void showAnnualStats(String name){
        System.out.println( userController.showAnnualStats(name) );
    }
    
    public void showMonthlyStats(String name){
        System.out.println( userController.showAnnualStats(name) );
    }
    
    public void showAnnualStats(){
        System.out.println( userController.showAnnualStats() );
    }
    
     public void showMonthlyStats(){
        System.out.println( userController.showMonthlyStats() );
    }
    
    public void getStatsOption(){
        System.out.println("Choose one of the following options.");
        FitnessUM.printStatsOptions();
        int option = Scan.menuOption(0, 2);
        this.getStatsPrompt()[option].exec();
    }
    
    public void getAddActivityOption(){
        System.out.println("Choose one of the following options.");
        FitnessUM.printActivities();
        int option = Scan.menuOption(0, 6);
        this.getAddActivityPrompt()[option].exec();
    }

    public void getStatsTypeOption(){
        System.out.println("Choose one of the following options.");
        System.out.println("0.Go Back");
        System.out.println("1.Annual Statistics");
        System.out.println("2.Monthly Statistics");
        int option = Scan.menuOption(0,2);
        this.getStatsTypePrompt()[option].exec();
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
    
    public void myActivityLog(){
        ArrayList<Activity> list = userController.getMostRecentActivities();
        
        System.out.print(list.toString()+"\n");
    }
    
    private Prompt[] getAddActivityPrompt(){
        final FitnessUM app = this;
        
        return new Prompt[]{
            new Prompt() { public void exec() { return;} },
            new Prompt() { public void exec() { app.addCycling();} },
            new Prompt() { public void exec() { app.addKayaking();} },
            new Prompt() { public void exec() { app.addKendo();} },
            new Prompt() { public void exec() { app.addRunning();} },
            new Prompt() { public void exec() { app.addSkating();} },
            new Prompt() { public void exec() { app.addSwimming();} }
        };
    }
    
    public void addCycling(){
        GregorianCalendar startDate = Scan.dateWithHours("When did you practice this activity?(dd-mm-yyyy)", "When did you start (hh:mm:ss)");
        GregorianCalendar endDate = Scan.time("When did you finish? (hh:mm:ss)");
        endDate.set(startDate.get(Calendar.YEAR), startDate.get(Calendar.MONTH), startDate.get(Calendar.DATE));
        long duration = endDate.getTimeInMillis() - startDate.getTimeInMillis();
        
        int distance = Scan.scanInt("What was the distance? (meters)");
        int altitude = Scan.scanInt("What was the altitude? (meters)");
        this.listWeatherOptions();
        int weather = Scan.scanInt(this.listWeatherOptions());
        
        this.userController.addActivity( new Cycling(startDate, duration, distance, altitude, weather));
    }
    
    public void addKayaking(){
        GregorianCalendar startDate = Scan.dateWithHours("When did you practice this activity?(dd-mm-yyyy)", "When did you start (hh:mm:ss)");
        GregorianCalendar endDate = Scan.time("When did you finish? (hh:mm:ss)");
        endDate.set(startDate.get(Calendar.YEAR), startDate.get(Calendar.MONTH), startDate.get(Calendar.DATE));
        long duration = endDate.getTimeInMillis() - startDate.getTimeInMillis();
        int distance = Scan.scanInt("What was the distance? (meters)");
        this.listWeatherOptions();
        int weather = Scan.scanInt(this.listWeatherOptions());
        
        this.userController.addActivity( new Kayaking(startDate, duration, distance, weather));
    }
    
    public void addKendo(){
        GregorianCalendar startDate = Scan.dateWithHours("When did you practice this activity?(dd-mm-yyyy)", "When did you start (hh:mm:ss)");
        GregorianCalendar endDate = Scan.time("When did you finish? (hh:mm:ss)");
        endDate.set(startDate.get(Calendar.YEAR), startDate.get(Calendar.MONTH), startDate.get(Calendar.DATE));
        long duration = endDate.getTimeInMillis() - startDate.getTimeInMillis();
        
        
                    System.out.println(endDate.get(Calendar.MONTH) + " " + endDate.get(Calendar.DATE));
            System.out.println(new SimpleDateFormat(" dd 'days' HH 'hours' mm 'minutes and' ss 'seconds' ").format( endDate.getTime() ));
        
        
        
        this.userController.addActivity( new Kendo(startDate, duration));
    }
    
    public void addRunning(){
        GregorianCalendar startDate = Scan.dateWithHours("When did you practice this activity?(dd-mm-yyyy)", "When did you start (hh:mm:ss)");
        GregorianCalendar endDate = Scan.time("When did you finish? (hh:mm:ss)");
        endDate.set(startDate.get(Calendar.YEAR), startDate.get(Calendar.MONTH), startDate.get(Calendar.DATE));
        long duration = endDate.getTimeInMillis() - startDate.getTimeInMillis();
        
        int distance = Scan.scanInt("What was the distance? (meters)");
        int altitude = Scan.scanInt("What was the altitude? (meters)");
        this.listWeatherOptions();
        int weather = Scan.scanInt(this.listWeatherOptions());
        
        this.userController.addActivity( new Running(startDate, duration, distance, altitude, weather));
    }
    
    public void addSkating(){
        GregorianCalendar startDate = Scan.dateWithHours("When did you practice this activity?(dd-mm-yyyy)", "When did you start (hh:mm:ss)");
        GregorianCalendar endDate = Scan.time("When did you finish? (hh:mm:ss)");
        endDate.set(startDate.get(Calendar.YEAR), startDate.get(Calendar.MONTH), startDate.get(Calendar.DATE));
        long duration = endDate.getTimeInMillis() - startDate.getTimeInMillis();
        
        this.userController.addActivity( new Skating(startDate, duration));
    }
    
    public void addSwimming(){
        GregorianCalendar startDate = Scan.dateWithHours("When did you practice this activity?(dd-mm-yyyy)", "When did you start (hh:mm:ss)");
        GregorianCalendar endDate = Scan.time("When did you finish? (hh:mm:ss)");
        
        endDate.set(startDate.get(Calendar.YEAR), startDate.get(Calendar.MONTH), startDate.get(Calendar.DATE));
        
        long duration = endDate.getTimeInMillis() - startDate.getTimeInMillis();
        int distance = Scan.scanInt("What was the distance? (meters)");
        
        this.userController.addActivity( new Swimming(startDate, duration, distance));
    }
        
                
    /** Scans the user for gender, height, weight, birth date and favorite sport
     * @return u UserInfo containing scanned information
     */
                
    public static UserInfo userInfo(){
        UserInfo u = new UserInfo();
        u.setGender( Scan.gender() );
        u.setHeight( Scan.height() );
        u.setWeight( Scan.weight() );
        u.setBirthDate( Scan.date("When were you born? (dd-mm-yyyy)") );
        u.setFavoriteSport( Scan.sport() );

        return u;
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
        int option = Scan.menuOption(0, 7);
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
            new Prompt() { public void exec() { app.viewFriendRequests(); } },
            new Prompt() { public void exec() { app.listFriends(); }},
            new Prompt() { public void exec() { app.searchUser(); }},
            new Prompt() { public void exec() { app.myActivityLog(); }},
            new Prompt() { public void exec() { app.getAddActivityOption(); } },
            new Prompt() { public void exec() { app.getStatsOption(); } }
        };
    }

    public Prompt[] getStatsPrompt(){
        final FitnessUM app = this;
        final ArrayList<String> activityList = new ArrayList( Arrays.asList(app.activities) );
        return new Prompt[]{
            new Prompt(){ public void exec(){ return;}},
            new Prompt() { public void exec() { app.getStatsTypeOption(); } },
            new Prompt() { public void exec() { ( new StatsNavigator(activityList) ).navigate(); } }
        };
    }

    private static Prompt[] getDevPrompt() {
        return new Prompt[] {
            new Prompt() { public void exec() { System.out.println("\nBye bye."); } },
            new Prompt() { public void exec() { new FitnessUM( new Seed().generate() ).run(); } },
            new Prompt() { public void exec() { System.out.println("\nFunction yet to be implemented\n"); FitnessUM.devPrompt(); } }
        };
     }

     public Prompt[] getStatsTypePrompt(){
        final FitnessUM app = this;
        return new Prompt[]{
            new Prompt(){ public void exec(){ return;}},
            new Prompt() { public void exec() { app.showAnnualStats(); } },
            new Prompt() { public void exec() { app.showMonthlyStats(); } }
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

    private static void printActivityCategories() {
        int i = 0;
        for (String s : FitnessUM.activityCategories)
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
    
    private static void printActivities() {
        int i = 0;
        for (String s : FitnessUM.activities)
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
