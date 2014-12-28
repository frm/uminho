/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers;

import data.RepositoryFactory;
import data.UserNotFoundException;
import models.User;
import models.Session;
import data.UsersRepository;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author frmendes
 */
public class UsersController {
    
    private UsersRepository users;
    
    public UsersController() {
        users = RepositoryFactory.getUsersRepository();
    }
    
    public Session authenticate(final String username, final String password) throws UserNotFoundException {
        
        Map<String, Object> params = new HashMap<String, Object>() {{
            put("username", username);
            put("password", User.passwordHash(password));
        }};
        
        return new Session(users.findBy(params).get(0));
    }
}
