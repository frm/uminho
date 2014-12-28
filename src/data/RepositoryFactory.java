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
    
    private static UsersRepository usersRepository;
    
    // TODO create valid login credentials
    private static final String USERNAME = System.getenv("MY_SQL_UN");
    private static final String PASSWORD = System.getenv("MY_SQL_PW");
    private static final String URL = "url";
    
    public RepositoryFactory() {
        usersRepository = new UsersRepository(getURL(), USERNAME, PASSWORD);
    }
    
    // TODO change this to generate valid mysql db url
    public static String getURL() {
        return URL;
    }
    
    public static UsersRepository getUsersRepository() {
        if (usersRepository == null)
            usersRepository = new UsersRepository(getURL(), USERNAME, PASSWORD);
        
        return usersRepository;
    }
    
}
