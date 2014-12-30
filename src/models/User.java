/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import org.apache.commons.codec.digest.DigestUtils;


/**
 *
 * @author frmendes
 */
public class User {
    private String name;
    private String username;
    private String password;
    private int id;
    
    public User(String name, String username, String password, int id) {
        this.name = name;
        this.username = username;
        this.password = User.passwordHash(password);
        this.id = id;
    }
    
    /**
     * Gets user unique identifier
     * @return unique identifier
      */
    public int getId() {
        return id;
    }

    /**
     * Sets user unique identifier
     * @param id new identifier
     */
    public void setId(int id) {
        this.id = id;
    }
    
    /**
     * Get name
     * @return User name
     */
    public String getName() {
        return name;
    }

    /**
     * Set name
     * @param name new name
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Get username
     * @return User's username
     */
    public String getUsername() {
        return username;
    }

    /**
     * Set username
     * @param username new username
     */
    public void setUsername(String username) {
        this.username = username;
    }

    /**
     * Update password
     * @param password new password
     */
    public void setPassword(String password) {
        this.password = User.passwordHash(password);
    }
    
    /**
     * Generates the password hash for a given password
     * @param password
     * @return hashed version of the password
     */
    public static String passwordHash(String password) {
        return DigestUtils.sha256Hex(password);
    }
}
