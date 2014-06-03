import java.util.HashMap;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author tiago
 */
public class Achievements {
    private HashMap<String,Boolean> achievements;
    
    //constructors
    public Achievements(){
        this.achievements = new HashMap<String,Boolean>();
    }
    
    public Achievements(HashMap<String,Boolean> ach){
        this.achievements = cloneAchievements(ach);
    }
    
    public Achievements(Achievements a){
        this.achievements = a.getAchievements();
    }
    
    

    //getters & setters
    public HashMap<String,Boolean> getAchievements(){
        return cloneAchievements(this.achievements);
    }
    
    public void setAchievements(HashMap<String,Boolean> a){
        this.achievements = cloneAchievements(a);
    }
    
    //methods
    public HashMap<String,Boolean> cloneAchievements(HashMap<String,Boolean> a){
        HashMap<String,Boolean> aux = new HashMap<String,Boolean>();
        aux.putAll(a);
        return aux;
    }
    
    public void addAchievement(String s, Boolean b){
        this.achievements.put(s, b);
    }
    
    public void removeAchievement(String s){
        this.achievements.remove(s);
    }
    
    //public boolean doneAchievement(){return true;}
    
    public void populateAchievs(){
        this.achievements.put("Commited",false);
        this.achievements.put("Expert",false);
        this.achievements.put("Tough",false);
        this.achievements.put("Tougher",false);
        this.achievements.put("Hard as nails",false);
        this.achievements.put("Fat Burner",false);
        this.achievements.put("Fat Destroyer",false);
        this.achievements.put("Fat Slayer",false);
        this.achievements.put("Baby Steps",false);
        this.achievements.put("Ear Pop",false);
    }
    //essentials
    public Achievements clone(){
        return new Achievements(this);
    }
    
    public boolean equals(Object o){
        if (this == o) return true;
        
        if (o == null || this.getClass() != o.getClass()) return false;
        
        Achievements ach = (Achievements) o;
        
        return this.achievements.equals(ach.getAchievements());
    }
    
    public String toSrting(){
        return this.achievements.toString();
    }
}
