import java.util.Hashtable;

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
    private Hashtable<String,Boolean> achievements;
    
    //constructors
    public Achievements(){
        this.achievements = new Hashtable<String,Boolean>();
    }
    
    public Achievements(Hashtable<String,Boolean> ach){
        this.achievements = cloneAchievements(ach);
    }
    
    public Achievements(Achievements a){
        this.achievements = a.getAchievements();
    }
    
    
    public Hashtable<String,Boolean> cloneAchievements(Hashtable<String,Boolean> a){
        Hashtable<String,Boolean> aux = new Hashtable<String,Boolean>();
        aux.putAll(achievements);
        return aux;
    }
    
    public Hashtable<String,Boolean> getAchievements(){
        return cloneAchievements(this.achievements);
    }
    
    public void setAchievements(Hashtable<String,Boolean> a){
        this.achievements = cloneAchievements(a);
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
        return achievements.toString();
    }
}
