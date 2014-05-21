
import java.text.SimpleDateFormat;
import java.util.GregorianCalendar;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author joaorodrigues
 */
public class StatEntry {
  private String name;
  private int totalCalories;
  private double avgCalories;
  private GregorianCalendar totalDuration;
  private GregorianCalendar avgDuration;
  private int nrEntries;
  
  public StatEntry(){
      this.totalCalories = 0;
      this.avgCalories = 0.0;
      this.name = "";
      this.totalDuration = new GregorianCalendar(0,0,0,0,0,0);
      this.avgDuration = new GregorianCalendar(0,0,0,0,0,0);
      this.nrEntries = 0;
  }
  
  public StatEntry(String name, int calories, GregorianCalendar duration){
      this.totalCalories = 0;
      this.avgCalories = 0.0;
      this.name = name;
      this.totalDuration = new GregorianCalendar(0,0,0,0,0,0);
      this.avgDuration = new GregorianCalendar(0,0,0,0,0,0);
      this.nrEntries = 1;
      
      updateStat(calories, duration);
  }

    public StatEntry(String name, int totalCalories, double avgCalories, GregorianCalendar totalDuration, GregorianCalendar avgDuration, int nrEntries) {
        this.name = name;
        this.totalCalories = totalCalories;
        this.avgCalories = avgCalories;
        this.totalDuration = (GregorianCalendar) totalDuration.clone();
        this.avgDuration = (GregorianCalendar) avgDuration.clone();
        this.nrEntries = nrEntries;
    }
    
    public StatEntry(Activity act){
        this.name = act.getClass().toString();
        this.totalCalories = 0;
        this.avgCalories = 0.0;
        this.totalDuration = new GregorianCalendar(0,0,0,0,0,0);
        this.avgDuration = new GregorianCalendar(0,0,0,0,0,0);
        this.nrEntries = 1;
        
        updateStat(act);
    }

    public StatEntry(StatEntry se) {
        this.name = se.getName();
        this.totalCalories = se.getTotalCalories();
        this.avgCalories = se.getAvgCalories();
        this.totalDuration = se.getTotalDuration();
        this.avgDuration = se.getAvgDuration();
        this.nrEntries = se.getNrEntries();
    }

    public String getName() {
        return name;
    }

    public int getTotalCalories() {
        return totalCalories;
    }

    public double getAvgCalories() {
        return avgCalories;
    }

    public GregorianCalendar getTotalDuration() {
        return (GregorianCalendar) totalDuration.clone();
    }

    public GregorianCalendar getAvgDuration() {
        return (GregorianCalendar) avgDuration.clone();
    }

    public int getNrEntries() {
        return nrEntries;
    }
    

    public void setName(String name) {
        this.name = name;
    }

    public void setTotalCalories(int totalCalories) {
        this.totalCalories = totalCalories;
    }

    public void setAvgCalories(double avgCalories) {
        this.avgCalories = avgCalories;
    }

    public void setTotalDuration(GregorianCalendar totalDuration) {
        this.totalDuration = (GregorianCalendar) totalDuration.clone();
    }

    public void setNrEntries(int nrEntries) {
        this.nrEntries = nrEntries;
    }
    
    

    public void setAvgDuration(GregorianCalendar avgDuration) {
        this.avgDuration = (GregorianCalendar) avgDuration.clone();
    }
    
    public void updateStat(Activity act){
        int calories = act.getCalories();
        GregorianCalendar duration = act.getDuration();
        
        this.nrEntries++;
        this.totalCalories += calories;
        this.avgCalories = this.totalCalories/this.nrEntries;
        
        this.totalDuration.setTimeInMillis( this.totalDuration.getTimeInMillis() + duration.getTimeInMillis() );
        this.totalDuration.setTimeInMillis( this.totalDuration.getTimeInMillis()/this.nrEntries );
    }
    
    public void updateStat(int calories, GregorianCalendar duration){
        this.nrEntries++;
        this.totalCalories += calories;
        this.avgCalories = this.totalCalories/this.nrEntries;
        
        this.totalDuration.setTimeInMillis( this.totalDuration.getTimeInMillis() + duration.getTimeInMillis() );
        this.totalDuration.setTimeInMillis( this.totalDuration.getTimeInMillis()/this.nrEntries );
    }
    
    public StatEntry clone(){
        return new StatEntry(this);
    }

    
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        
        StatEntry se = (StatEntry) obj;
        return (this.name.equals(se.getName()) && this.avgCalories == se.getAvgCalories() && this.totalCalories == se.getTotalCalories() && this.avgDuration.equals(se.getAvgDuration()) && this.totalDuration.equals(se.getTotalDuration()) );
    }

    public String toString(){
        StringBuilder result = new StringBuilder();
        
        result.append("\n\nSport: ");
        result.append(this.name);
        result.append("\nNumber of times practiced: ");
        result.append(this.nrEntries);
        result.append("\nAverage of calories burned per session: ");
        result.append(this.avgCalories);
        result.append("\nTotal of calories burned: ");
        result.append(this.totalCalories);
        result.append("\nAverage duration of sessions: ");
        result.append(new SimpleDateFormat("HH 'hours' mm 'minutes and' ss 'seconds' ").format( this.avgDuration.getTime() ));
        result.append("\nTotal of time spent doing this activity: ");
        result.append(new SimpleDateFormat("HH 'hours' mm 'minutes and' ss 'seconds' ").format( this.totalDuration.getTime() ));
        
        return result.toString();
    } 
}
