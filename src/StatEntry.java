
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
  private double totalCalories;
  private double avgCalories;
  private long totalDuration;
  private long avgDuration;
  private int nrEntries;
  
  public StatEntry(){
      this.totalCalories = 0;
      this.avgCalories = 0.0;
      this.name = "";
      this.totalDuration = 0;
      this.avgDuration = 0;
      this.nrEntries = 0;
  }
  
  public StatEntry(String name, int calories, long duration){
      this.totalCalories = 0;
      this.avgCalories = 0.0;
      this.name = name;
      this.totalDuration =0;
      this.avgDuration =0;
      this.nrEntries = 0;
      
      updateStat(calories, duration);
  }

    public StatEntry(String name, int totalCalories, double avgCalories, long totalDuration, long avgDuration, int nrEntries) {
        this.name = name;
        this.totalCalories = totalCalories;
        this.avgCalories = avgCalories;
        this.totalDuration = totalDuration;
        this.avgDuration = avgDuration;
        this.nrEntries = nrEntries;
    }
    
    public StatEntry(Activity act){
        this.name = act.getClass().getSimpleName();
        this.totalCalories = 0;
        this.avgCalories = 0.0;
        this.totalDuration = 0;
        this.avgDuration = 0;
        this.nrEntries = 0;
        
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

    public double getTotalCalories() {
        return totalCalories;
    }

    public double getAvgCalories() {
        return avgCalories;
    }

    public long getTotalDuration() {
        return  totalDuration;
    }

    public long getAvgDuration() {
        return avgDuration;
    }

    public int getNrEntries() {
        return nrEntries;
    }
    
    public static String formatMillis(long millis){
        int seg = (int)millis/1000;
        
        int min = seg/60;
        seg = seg%60;
        
        int hr = min/60;
        min = min%60;
        
        return (hr + "h" + min + "m" + seg + "s");
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

    public void setTotalDuration(long totalDuration) {
        this.totalDuration = totalDuration;
    }

    public void setNrEntries(int nrEntries) {
        this.nrEntries = nrEntries;
    }
    
    

    public void setAvgDuration(long avgDuration) {
        this.avgDuration = avgDuration;
    }
    
    public void updateStat(Activity act){
        double calories = act.getCalories();
        long duration = act.getDuration();
        
        this.nrEntries++;
        this.totalCalories += calories;
        this.avgCalories = this.totalCalories/this.nrEntries;
        
        this.totalDuration += duration;
        this.avgDuration = this.totalDuration/this.nrEntries;
        
    }
    
    public void updateStat(int calories, long duration){
        this.nrEntries++;
        this.totalCalories += calories;
        this.avgCalories = this.totalCalories/this.nrEntries;
        
        this.totalDuration += duration;
        this.avgDuration = this.totalDuration/this.nrEntries;
        
        
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
        return (this.name.equals(se.getName()) && this.avgCalories == se.getAvgCalories() && this.totalCalories == se.getTotalCalories() && this.avgDuration == se.getAvgDuration() && this.totalDuration == se.getTotalDuration() );
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
        result.append(StatEntry.formatMillis(this.avgDuration));
        result.append("\nTotal of time spent doing this activity: ");
        result.append(StatEntry.formatMillis(this.totalDuration));
        
        return result.toString();
    } 
}
