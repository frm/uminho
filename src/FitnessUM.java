
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeMap;

/**
 *
 * @author frmendes
 */

public class FitnessUM {

    private boolean active;
    private UserController userController;
    private EventController eventController;


   private static final String[] startOptions = { "Exit", "Register", "Login" };

   private static final String[] mainOptions = {

       "Logout", "My Profile", "Friend Requests", "Friend List", "Friends Feed", "Search User", "My Activity Log","My Events",
       "Add New Activity Session", "Show My Statistics", "Update Settings", "Show My Records", "Show Event List", "Search Events", "Delete Account"
   };

    private static final String[] activityCategories = {
       "Go Back", "Simple Activities", "Distance Activities", "Altitude Activities"
   };

   private static final String[] statsOptions = {
       "Go Back", "Statistics Overview", "Statistics for a given year", "Statistics for a given month"
   };

   private static final String[] activities = {
       "Go Back", "Cycling", "Kayaking", "Kendo", "Running", "Skating", "Swimming"
   };
   
   private static final String[] eventActivities = {
       "Cycling", "Kayaking", "Running", "Swimming"
   };

   private static final String[] adminOptions = {
       "Logout", "Add Admin", "Delete User", "Add Event"
   };
   
   private static final String[] listEventsOptions = {
       "Go Back", "List All Events", "List Upcoming Events", "List Upcoming Events of a Certain Activity Type"
   };



    /** Empty constructor
     */
    public FitnessUM() {
        this.userController = new UserController();
        this.eventController = new EventController();
    }

    /** Parameterized constructor
     * @param userController
     * @param eventController
     */
    public FitnessUM(UserController userController, EventController eventController) {
        this.userController = userController.clone();
        this.eventController = eventController.clone();
    }

    /** Copy constructor
     * @param fit existing FitnessUM app
     */
    public FitnessUM(FitnessUM fit) {
        this.userController = fit.getUserController();
        this.eventController = fit.getEventController();
    }

    /** Getter for logged in user ID
     * @return logged in user ID
     */
    public User getUserCurrentUser() {
        return this.userController.getCurrentUser();
    }

    /**
     *
     * @return
     */
    public UserController getUserController() {
        return this.userController.clone();
    }

    /**
     *
     * @param uc
     */
    public void setUserController(UserController uc) {
        this.userController = uc.clone();
    }

    /**
     *
     * @return
     */
    public EventController getEventController() {
        return eventController.clone();
    }

    /**
     *
     * @param eventController
     */
    public void setEventController(EventController eventController) {
        this.eventController = eventController.clone();
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
        try{
            this.userController.writeToFile("userData.sav");
            this.eventController.writeToFile("eventData.sav");
            this.active = false;
        }
        catch(IOException e){System.out.println("Write error");}
    }

    /** Scans for information and saves the user into the database
      */
    public void registerUser() {
        String name = Scan.name("\nFirst name: ") + " " + Scan.name("\nLast name: ");
        String email = Scan.email();

        while ( ! this.userController.validUserEmail(email) ) {
            System.out.println("Email is already taken");
            email = Scan.email();
        }

        String password = Scan.password();
        UserInfo info = readUserInfo();

        this.userController.registerUser(name, email, password, info);
        this.userController.loginUser(email, password);
    }

    /**
     * Registers an admin
     */
    public void registerAdmin() {
        String name = Scan.name("\nAdmin name: ");
        String email = Scan.email();

        while( ! this.userController.validAdminEmail(email) ) {
            System.out.println("Invalid email");
            email = Scan.email();
        }

        String password = Scan.password();

        this.userController.registerAdmin(name, password, email);
    }

    /**
     * Scans the user for update fields
     */
    public void updateUser() {
        System.out.println("You are about to update your settings.\nIf you do not wish to update a particular field, simply press Enter or input 0 in numeric fields.");

        String name = Scan.updateName("\nFirst name: ") + " " + Scan.updateName("\nLast name: ");
        String email = Scan.updateEmail();

        while ( email.length() > 0 && ! this.userController.validUserEmail(email) ) {
            System.out.println("Email is already taken");
            email = Scan.updateEmail();
        }

        String password = Scan.updatePassword();
        UserInfo info = readUpdateInfo();

        this.userController.updateUser(name, email, password, info);
    }

    private void searchUserByName() {
        String name = Scan.name("Enter a name:");
        new SearchUserNavigator( this.userController.nameSearch(name), this ).navigate();
    }

    private void searchUserByEmail() {
        String email = Scan.email();
        new SearchUserNavigator( this.userController.emailSearch(email), this ).navigate();
    }

    /**
     * Scans the user for a particular search, proceeding to do it
     */
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

    /**
     * Logs out a user
     */
    public void logoutUser() {
        if ( this.userController.isAdminLogin() )
            this.userController.logoutAdmin();
    }

    /** Scans for valid login info and sets the current_user
     */
    public void loginUser() {
        int nrAttempts = 0;
        boolean logged = false;

        while (nrAttempts < 3 && !logged) {
            String email = Scan.scanString("Enter email:");

            while ( !this.userController.existsEmail(email) ) {
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

    /**
     * Scans the user for email, so it can delete it
     */
    public void deleteUser() {
        String email = Scan.email();
        deleteByEmail(email);
    }
    
    /**
     * Prompts the user for delete account confirmation
     * @param email
     */
    public void deleteByEmail(String email){
        while( !this.userController.existsUser(email) ) {
            System.out.println("User does not exist");
            email = Scan.email();
        }

        if ( Scan.yesNo("Are you sure you want to delete user with given email?") ){
            User u = this.userController.getByEmail(email);
            this.userController.deleteUser(email);
            this.eventController.removeUser(u);
        }
    }
    
    /**
     * Deletes the current user account
     */
    public void deleteMyAccount(){
        deleteByEmail( this.userController.getCurrentUser().getEmail());
        run();
    }

    
    private void greet() {
        if( this.userController.getCurrentUser() != null ) { // If user is not admin
            System.out.println("\nWelcome "+ this.userController.getCurrentUser().getName() );
            if ( this.userController.hasFriendRequests() )
                System.out.println("You have friend requests!");
        }
    }

    /**
     * Returns a friend feed
     */
    public void friendsFeed(){
        try{
            Set<Tuple<String, Activity>> feed = this.userController.getFriendsFeed();
            for(Tuple<String, Activity> t : feed)
            System.out.println("\t###\nUser: " + t.getKey() + "\n" + t.getValue() + "\t###\n" );
            Scan.pressEnterToContinue();
        }
        catch(EmptyFeedException e){ 
            System.out.println("\nThe Feed is Empty");
            Scan.pressEnterToContinue();
        }
        
    }

    /**
     * Prints the user profile
     */
    public void userProfile() {
        System.out.println( this.userController.currentUserProfile() );
        Scan.pressEnterToContinue();
    }

    /**
     * Lists the user practiced activities
     */
    public void listPracticedActivities() {
        ArrayList<String> list = this.userController.getPracticedActivities();
        new RecordsNavigator(list, this.userController).navigate();
    }

    /**
     * Lists the user friends
     */
    public void listFriends() {
        new FriendListNavigator( this.userController.getFriendList(), this ).navigate();
    }

    /**
     * Sends a user friend request
     * @param u
     */
    public void addFriend(User u) {
        this.userController.sendFriendRequest(u);
    }

    /**
     * Deletes a friend
     * @param u
     */
    public void deleteFriend(User u) {
        this.userController.deleteFriend(u);
    }

    /**
     * Accepts a friend request
     * @param u
     */
    public void acceptFriend(User u) {
        this.userController.acceptFriendRequest(u);
    }

    /**
     * Rejects a friend request
     * @param u
     */
    public void rejectFriend(User u) {
        this.userController.rejectFriendRequest(u);
    }

    /**
     * Determines if the user has friend requests
     * @param u
     * @return
     */
    public boolean currentUserHasFriend(User u) {
        return this.userController.getCurrentUser().hasFriend(u);
    }

    /**
     * Lists the friend requests
     */
    private void viewFriendRequests() {
        new FriendRequestsNavigator( this.userController.getFriendRequests(), this ).navigate();
    }

    /**
     * Shows the statistics overview
     */
    public void showStatsOverview(){
        try{
            System.out.println(userController.showStatsOverview());
            Scan.pressEnterToContinue();
        }
        catch(StatsNotAvailableException s){
            System.out.println("No Stats Available\n");
            Scan.pressEnterToContinue();
        }
    }

    /**
     * Shows the annual statistics, prompting the user for the year
     */
    public void showAnnualStats(){
        int year = Scan.scanInt("Insert the year you want to check.");
        try{
        System.out.println( userController.showAnnualStats(year) );
        }

        catch(StatsNotAvailableException s){System.out.println("No Stats Available\n");}

    }

    /**
     * Shows the monthly statistics, prompting the user for month and year
     */
    public void showMonthlyStats(){
        int year = Scan.intInRange("Insert the year you want to check.", 0, (new GregorianCalendar()).get(Calendar.YEAR) );
        int month = Scan.intInRange("Insert the month (number).", 1, 12);
        try{
           System.out.println( userController.showMonthlyStats(year, month) );
        } catch (StatsNotAvailableException s) {
            System.out.println("No Stats Available\n");
        }
    }

    /**
     * Removes a given activity
     * @param act
     */
    public void removeActivity(Activity act){
        this.userController.removeActivity(act);
    }

    /**
     * Prompts the user for add Activity options
     */
    public void getAddActivityOption(){
        System.out.println("Choose one of the following options.");
        FitnessUM.printActivities();
        int option = Scan.menuOption(0, 6);
        this.getAddActivityPrompt()[option].exec();
    }
   
    /**
     * Returns the statistics type option
     */
    public void getStatsTypeOption(){
        System.out.println("Choose one of the following options.");
        FitnessUM.printStatsOptions();
        int option = Scan.menuOption(0,3);
        this.getStatsTypePrompt()[option].exec();
    }

    /**
     * Lists the available weather options
     * @param message
     * @return
     */
    public static String listWeatherOptions(String message){
        String[] list = Weather.weatherStates;
        StringBuilder result = new StringBuilder();
        result.append(message);
        for( String w: list){
            result.append(Weather.getIndexOf(w)).append(".").append(w).append("\n");
        }
        return result.toString() ;
    }

    /**
     * Shows the user's ten most recent activities
    */
    public void myActivityLog(){
        ArrayList<Activity> list = userController.getMostRecentActivities();

        new ActivityNavigator(this,list).navigate();
    }
    
    /**
     * Returns a prompt to add an activity
     * @return 
     */

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


    /**
     *
     * @return
     */
    public GregorianCalendar getStartDate(){
        GregorianCalendar date = Scan.dateWithHours("When did you practice this activity?(dd-mm-yyyy)", "When did you start (hh:mm:ss)");
        if( this.userController.getCurrentUser().beforeBorn(date) || date.compareTo( new GregorianCalendar() ) > 0 ) {
            System.out.println("\nInvalid date\n");
            this.getStartDate();
        }
        return date;
    }

    /**
     *
     * @param startDate
     * @return
     */
    public long getDuration(GregorianCalendar startDate){
        GregorianCalendar endDate = Scan.time("When did you finish? (hh:mm:ss)");
        endDate.set(startDate.get(Calendar.YEAR), startDate.get(Calendar.MONTH), startDate.get(Calendar.DATE));
        return endDate.getTimeInMillis() - startDate.getTimeInMillis();
    }

    /**
     *
     * @return
     */
    public AltitudeActivity getAltitudeActivityData(){
        GregorianCalendar startDate = new GregorianCalendar();
        long duration = 0;

        while(duration <= 0){
            startDate = getStartDate();
            duration = getDuration(startDate);
            if (duration <= 0)
                System.out.println("Invalid finish time\n");
        }

        int distance = Scan.scanInt("\nWhat was the distance? (meters)");
        int altitude = Scan.scanInt("\nWhat was the altitude? (meters)");

        return (AltitudeActivity) (new Cycling(startDate, duration, distance, altitude));
    }

    /**
     *
     * @return
     */
    public DistanceActivity getDistanceActivityData(){
        GregorianCalendar startDate = new GregorianCalendar();
        long duration = 0;

        while(duration <= 0){
            startDate = getStartDate();
            duration = getDuration(startDate);
            if (duration <= 0)
                System.out.println("Invalid finish time\n");
        }

        int distance = Scan.scanInt("What was the distance? (meters)");

        return (DistanceActivity) (new Swimming(startDate, duration, distance));
    }

    /**
     *
     * @return
     */
    public Activity getBasicActivityData(){
        GregorianCalendar startDate = new GregorianCalendar();
        long duration = 0;

        while(duration <= 0){
            startDate = getStartDate();
            duration = getDuration(startDate);
            if (duration <= 0)
                System.out.println("Invalid finish time\n");
        }

        return (Activity) (new Kendo(startDate, duration));
    }

    /**
     *
     */
    public void addCycling(){
        AltitudeActivity model = getAltitudeActivityData();
        int weather = Scan.intInRange( this.listWeatherOptions("\nHow was the weather?\n"), 0, Weather.weatherStates.length - 1);

        if( ! this.userController.addActivity( new Cycling(model, weather) ) ) {
            System.out.println("\nInvalid activity");
            Scan.pressEnterToContinue();
        }
    }

    /**
     *
     */
    public void addKayaking(){
        DistanceActivity model = getDistanceActivityData();
        int weather = Scan.intInRange( this.listWeatherOptions("\nHow was the weather?\n"), 0, Weather.weatherStates.length - 1);

        if( !this.userController.addActivity( new Kayaking(model, weather) ) ) {
            System.out.println("\nInvalid activity");
            Scan.pressEnterToContinue();
        }
    }

    /**
     *
     */
    public void addKendo() {
        Activity model = getBasicActivityData();

        if (! this.userController.addActivity( new Kendo(model) ) ) {
            System.out.println("\nInvalid activity");
            Scan.pressEnterToContinue();
        }
    }

    /**
     *
     */
    public void addRunning(){
        AltitudeActivity model = getAltitudeActivityData();
        int weather = Scan.intInRange( this.listWeatherOptions("\nHow was the weather?\n"), 0, Weather.weatherStates.length - 1);

        if( !this.userController.addActivity( new Running(model, weather) ) ) {
            System.out.println("\nInvalid activity");
            Scan.pressEnterToContinue();
        }
    }

    /**
     *
     */
    public void addSkating(){
        Activity model = getBasicActivityData();
        if( !this.userController.addActivity( new Skating(model) ) ) {
            System.out.println("\nInvalid activity");
            Scan.pressEnterToContinue();
        }
    }

    /**
     *
     */
    public void addSwimming() {
        DistanceActivity model = getDistanceActivityData();
        if( !this.userController.addActivity( new Swimming(model) ) ) {
            System.out.println("\nInvalid activity");
            Scan.pressEnterToContinue();
        }
    }
    
    /**Starts a navigator, asking the admin for the event type
     *
     */
    public void addEvent(){
        ArrayList<String> activities = new ArrayList<String>( Arrays.asList(this.eventActivities) );
        ( new EventTypeNavigator(activities, this)).navigate();
    }

    /**
     *
     */
    public void searchEvent(){
        String terms = ( Scan.scanString("Insert your search terms") ).trim();
        ArrayList<String> list = this.eventController.searchEvent(terms);
        (new SearchEventNavigator(list, this.eventController)).navigate();
    }
    
    /**
     *
     */
    public void getListEventsOption() {
        this.startup();
        System.out.println("Choose one of the following options.");
        FitnessUM.printListEventsOptions();
        int option = Scan.menuOption(0, 3);
        this.getListEventsPrompt()[option].exec();
    }
    
    /**
     *
     * @param e
     */
    public void leaveEvent(Event e) {
        try {
            this.userController.leaveEvent( e.getId() );
            this.eventController.removeUser(this.userController.getCurrentUser(), e);
        } catch (InvalidParticipantException ex) {
            System.out.println("Invalid Participant");
        } catch (ActivityNotAvailableException ex) {
            System.out.println("Activity Not Available");
        } catch (InexistingUserException ex) {
            System.out.println("User does not participate");
        }
    }
    
    /**
     *
     */
    public void myEvents(){
        HashSet<Integer> ids = this.userController.getCurrentUser().getEvents().getEvents();
        ArrayList<Event> events = new ArrayList<Event>();
        for(Integer i: ids){
            events.add( this.eventController.getEventById(i) );
        }
        ( new EventNavigator(events, this)).navigate();
    }
    
    /**
     *
     * @param e
     * @return
     */
    public boolean userIsInEvent(Event e){
        return this.userController.getCurrentUser().isInEvent(e);
    }
    
    /**
     *
     * @param e
     */
    public void joinEvent(Event e){
        try {
                this.eventController.addUser(this.userController.getCurrentUser(), e);
                this.userController.joinEvent( e.getId() );
        } catch (InvalidParticipantException ex) {
            System.out.println("Invalid Participant");
        } catch (ActivityNotAvailableException ex) {
            System.out.println("Activity Not Available");
        } catch (LateForEventException ex) {
            System.out.println("You are too late to sign up for the event");
        }
    }
    
    /**
     *
     */
    public void listEvents(){
        getListEventsOption();
    }

    /**
     *
     */
    public void listAllEvents(){
            ArrayList<Event> events = this.eventController.getEventList();
            (new EventNavigator(events, this)).navigate();
    }
    
    /**
     *
     */
    public void listUpcomingEvents(){
        ArrayList<Event> events = this.eventController.getUpcomingEvents();
        ( new EventNavigator(events, this)).navigate();
    }
    
    /**
     *
     * @param name
     */
    public void listUpcomingEvents(String name){

        ArrayList<Event> events = this.eventController.getUpcomingEvents(name);
        (new EventNavigator(events, this)).navigate();
    }

    /** Scans the admin for event details, saving the event in the event controller
     * @param s
     */
    public void getEventInfo(String s) {
        GregorianCalendar date = new GregorianCalendar();
        GregorianCalendar signup = new GregorianCalendar();
        
        String name = Scan.scanString("\nWhat is the name of the event?");
        
        while( !this.eventController.validateEventNameUniqueness(name) ) {
            System.out.println("Event name has to be unique");
            name = Scan.scanString("\nWhat is the name of the event?");
        }
        
        boolean valid = false;
        while(!valid){
            date = Scan.eventDate("\nWhat's the event date? (dd-mm-yyyy)");
            signup = Scan.eventDate("\nWhat's the sign-up limit date? (dd-mm-yyyy)");
            if( signup.getTimeInMillis() <= date.getTimeInMillis())
                valid = true;
            else System.out.println("Invalid event or signup date");
        }    
        
        String location = Scan.scanString("\nWhat's the location?");
        
        
        
        int capacity = Scan.intInRange("What's the event capacity?",1, Integer.MAX_VALUE);
        int weather = Scan.intInRange( this.listWeatherOptions("\nWhat's the weather forecast\n"), 0, Weather.weatherStates.length - 1);
        
        EventInfo info = new EventInfo(capacity, name, location, weather, date, signup);
        
       
        int distance = Scan.intInRange("What is the distance? (km)", 1, Integer.MAX_VALUE);

        Event e = new Event(s, distance, info);
        
        
	this.eventController.addEvent(e);
    }

    /** Scans the admin for event name, deleting the event
     */
    public void deleteEvent() {
    	System.out.println("Yet to be implemented");
    }

    /** Scans the user for gender, height, weight, birth date and favorite sport
     * @return u UserInfo containing scanned information
     */
    public static UserInfo readUserInfo(){
        UserInfo u = new UserInfo();
        u.setGender( Scan.gender() );
        u.setHeight( Scan.height() );
        u.setWeight( Scan.weight() );
        u.setBirthDate( Scan.date("When were you born? (dd-mm-yyyy)") );
        u.setFavoriteSport( Scan.sport() );

        return u;
    }
    
    /**
     *
     * @param e
     */
    public void simulateEvent(Event e) {
        int max = e.getDistance();
        int km = Scan.intInRange("Choose what kilometer you want to view: [1, " + max + "] (0 to exit)", 0, max);
        ArrayList<TreeMap<Long, User>> sim = e.simulate( this.userController.getUsersWithId( e.getParticipants() ) );
        
        while(km != 0) {
            System.out.println( Event.getSimulatedKm( sim.get(km - 1) ) );
            Scan.pressEnterToContinue();
            km = Scan.intInRange("Choose what kilometer you want to view: [1, " + max + "] (0 to exit)", 0, max);
        }
    }

    /**
     *
     */
    public static void importData(){
        FitnessUM app = new FitnessUM();
        UserController uc = new UserController();
        EventController ec = new EventController();
        try{
            uc.readFromFile("userData.sav");
            ec.readFromFile("eventData.sav");
            app.setUserController(uc);
            app.setEventController(ec);
            app.run();
        }
        catch(IOException e){System.out.println("Loading error\n");}
        catch(ClassNotFoundException e){System.out.println("Loading error\n");}
    }

    /** Scans the user for up to date height, weight and favorite sport
     * @return User Info containing scanned info
     */
    public static UserInfo readUpdateInfo() {
        UserInfo u = new UserInfo();
        u.setHeight( Scan.updateHeight() );
        u.setWeight( Scan.updateWeight() );
        u.setFavoriteSport( Scan.updateSport() );
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

    /** Shows the main options for regular users, reading the input for the options
     *  and launching the corresponding events accordingly
     */
    public void userInterpreter() {
        System.out.println( "Choose one of the following options.");
	    FitnessUM.printMainOptions();
        int option = Scan.menuOption(0, 14);
        this.getMainPrompt()[option].exec();
    }
    
    /**
     *
     */
    public void getEventTypeOption() {
        System.out.println( "What's the type of the event?");
	    FitnessUM.printActivities();
        int option = Scan.menuOption(0, 6);
        this.getEventTypePrompt()[option].exec();
    }

    /** Shows the main options for admin users, reading the input and launching events
     * Beware as these options include creating and destroying events as well destroying users
     */
    public void adminInterpreter() {
        System.out.println("You are on an admin account. We trust you know what you are doing.\nWith great power comes great responsability.\n");
        FitnessUM.printAdminOptions();
        int option = Scan.menuOption(0, 3);
        this.getAdminPrompt()[option].exec();
    }


    /** Controls the main flow of events.
     */
    public void run() {
        System.out.println("\nWelcome to FitnessUM");

        this.getStartOption();
        if( this.userController.isAdminLogin() )
            while( this.isActive() )
                this.adminInterpreter();

        else
            while( this.isActive() )
              this.userInterpreter();
    }

    private Prompt[] getStartPrompt() {
        final FitnessUM app = this;
        return new Prompt[] {
            new Prompt() { public void exec() { FitnessUM.devPrompt(); app.shutdown(); } },
            new Prompt() { public void exec() { app.registerUser();} },
            new Prompt() { public void exec() { app.loginUser();} }
        };
    }

    private Prompt[] getAdminPrompt() {
        final FitnessUM app = this;
        return new Prompt[] {
            new Prompt() { public void exec() { app.logoutUser(); app.run(); } },
            new Prompt() { public void exec() { app.registerAdmin(); } },
            new Prompt() { public void exec() { app.deleteUser(); } },
            new Prompt() { public void exec() { app.addEvent(); } }
        };
    }

    private Prompt[] getMainPrompt() {
        final FitnessUM app = this;
        return new Prompt[] {
            new Prompt() { public void exec() { app.logoutUser(); app.run(); } },
            new Prompt() { public void exec() { app.userProfile(); } },
            new Prompt() { public void exec() { app.viewFriendRequests(); } },
            new Prompt() { public void exec() { app.listFriends(); }},
            new Prompt() { public void exec() { app.friendsFeed(); }},
            new Prompt() { public void exec() { app.searchUser(); }},
            new Prompt() { public void exec() { app.myActivityLog(); }},
            new Prompt() { public void exec() { app.myEvents(); }},
            new Prompt() { public void exec() { app.getAddActivityOption(); } },
            new Prompt() { public void exec() { app.getStatsTypeOption(); } },
            new Prompt() { public void exec() { app.updateUser(); } },
            new Prompt() { public void exec() { app.listPracticedActivities(); } },
            new Prompt() { public void exec() { app.listEvents(); } },
            new Prompt() { public void exec() { app.searchEvent(); } },
            new Prompt() { public void exec() { app.deleteMyAccount(); } }
        };
    }

    private static Prompt[] getDevPrompt() {
        return new Prompt[] {
            new Prompt() { public void exec() { System.out.println("\nBye bye."); } },
            new Prompt() { public void exec() { new FitnessUM( new Seed().generate() ).run(); } },
            new Prompt() { public void exec() { importData(); } }
        };
     }

    /**
     *
     * @return
     */
    public Prompt[] getStatsTypePrompt(){
        final FitnessUM app = this;
        return new Prompt[]{
            new Prompt() { public void exec() { return; }},
            new Prompt() { public void exec() { app.showStatsOverview(); } },
            new Prompt() { public void exec() { app.showAnnualStats(); } },
            new Prompt() { public void exec() { app.showMonthlyStats(); } }
        };
    }
    
    /**
     *
     * @return
     */
    public Prompt[] getListEventsPrompt(){
        final FitnessUM app = this;
        return new Prompt[]{
            new Prompt() { public void exec() { return; }},
            new Prompt() { public void exec() { app.listAllEvents(); } },
            new Prompt() { public void exec() { app.listUpcomingEvents(); } },
            new Prompt() { public void exec() { app.getEventTypeOption(); } }
        };
    }
    
     private Prompt[] getEventTypePrompt(){
        final FitnessUM app = this;

        return new Prompt[]{
            new Prompt() { public void exec() { return;} },
            new Prompt() { public void exec() { listUpcomingEvents(  app.activities[1]);} },
            new Prompt() { public void exec() { listUpcomingEvents(  app.activities[2]);} },
            new Prompt() { public void exec() { listUpcomingEvents(  app.activities[3]);} },
            new Prompt() { public void exec() { listUpcomingEvents(  app.activities[4]);} },
            new Prompt() { public void exec() { listUpcomingEvents(  app.activities[5]);} },
            new Prompt() { public void exec() { listUpcomingEvents(  app.activities[6]);} }
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

    private static void printAdminOptions() {
        int i = 0;
        for(String s : FitnessUM.adminOptions)
            System.out.println(i++ + ". " + s);
    }
    
    private static void printListEventsOptions() {
        int i = 0;
        for (String s : FitnessUM.listEventsOptions)
            System.out.println(i++ + ". " + s);
    }


    public FitnessUM clone() {
        return new FitnessUM(this);
    }

    /**
     *
     * @param args
     */
    public static void main(String[] args) {
        System.out.println("Welcome to FitnessUM Dev Prompt.");
        FitnessUM.devPrompt();
    }
}
