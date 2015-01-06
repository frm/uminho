/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package data;

/**
 *
 * @author frmendes
 */
public class RepositoryFactory {
    
    private static VolunteersRepository volunteersRepository;
    private static ActivityRepository activityRepository;
    private static RepresentativeRepository representativeRepository;
    
    //private static final String USERNAME = System.getenv("HBT_USR");
    //private static final String PASSWORD = System.getenv("HBT_PW");
    private static final String USERNAME = "habitat";
    private static final String PASSWORD = "testuser123";
    private static final String URL = "jdbc:mysql://localhost/habitat";
    
    public RepositoryFactory() {
        volunteersRepository = new VolunteersRepository(getURL(), USERNAME, PASSWORD);
    }
    
    // TODO change this to generate valid mysql db url
    public static String getURL() {
        return URL;
    }
    
    public static VolunteersRepository getVolunteersRepository() {
        if (volunteersRepository == null)
            volunteersRepository = new VolunteersRepository(getURL(), USERNAME, PASSWORD);
        
        return volunteersRepository;
    }
    
    public static ActivityRepository getActivityRepository() {
        if (activityRepository == null)
            activityRepository = new ActivityRepository(getURL(), USERNAME, PASSWORD);
        
        return activityRepository;
    }
    
        public static RepresentativeRepository getRepresentativeRepository() {
        if (representativeRepository == null)
            representativeRepository = new RepresentativeRepository(getURL(), USERNAME, PASSWORD);
        
        return representativeRepository;
    }
}
