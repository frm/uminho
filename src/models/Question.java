/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

/**
 *
 * @author joaorodrigues
 */
public class Question {
    int id;
    boolean enabled;
    String text;

    public Question(boolean enabled, String text) {
        this.id = -1;
        this.enabled = enabled;
        this.text = text;
    }

    public Question() {
        this.id = -1;
        this.enabled = false;
        this.text = "";
    }
    
    
    
    
    public int getId() {
        return id;
    }

    public boolean isEnabled() {
        return enabled;
    }

    public String getText() {
        return text;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public void setText(String text) {
        this.text = text;
    }
    
    
}
