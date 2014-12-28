/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package models;

/**
 *
 * @author frmendes
 */
public class Session {
    
    private int id;
    
    public Session(User u) {
        this.id = u.getId();
    }
}
