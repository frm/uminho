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
    private int id;
    private boolean enabled;
    private String text;

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
    
    public void setId(int id) {
        this.id = id;
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

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        
        if(obj == null || this.getClass() != obj.getClass())
            return false;
        
        Question q = (Question) obj;
        
        return (q.getId() == id );
    }
}
