
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
public class DistanceStatEntry extends StatEntry{
    private int totalDistance;
    private double avgDistance;
    
    public DistanceStatEntry(){
        super();
        this.totalDistance = 0;
        this.avgDistance = 0.0;
    }

    public DistanceStatEntry(int totalDistance, double avgDistance) {
        this.totalDistance = totalDistance;
        this.avgDistance = avgDistance;
    }

    public DistanceStatEntry(String name, int calories, long duration, int totalDistance, double avgDistance) {
        super(name, calories, duration);
        this.totalDistance = totalDistance;
        this.avgDistance = avgDistance;
    }

    public DistanceStatEntry(String name, int totalCalories, double avgCalories, long totalDuration, long avgDuration, int nrEntries, int totalDistance, double avgDistance) {
        super(name, totalCalories, avgCalories, totalDuration, avgDuration, nrEntries);
        this.totalDistance = totalDistance;
        this.avgDistance = avgDistance;
    }

    public DistanceStatEntry(DistanceActivity act) {
        super(act);
        this.totalDistance = 0;
        this.avgDistance = 0.0;
        
        updateStat(act);
    }
    
    public DistanceStatEntry(DistanceStatEntry dse){
        super(dse);
        
        this.totalDistance = dse.getTotalDistance();
        this.avgDistance = dse.getAvgDistance();
    }
    
    
    public void updateStat(DistanceActivity act){
        super.updateStat(act);
        
        this.totalDistance += act.getDistance();
        this.avgDistance = this.totalDistance/this.getNrEntries();
    }
    
    public void updateStat(int calories, long duration, int distance){
        super.updateStat(calories, duration);
        
        this.totalDistance += distance;
        this.avgDistance = this.totalDistance/this.getNrEntries();
    }
    
    

    public int getTotalDistance() {
        return totalDistance;
    }

    public double getAvgDistance() {
        return avgDistance;
    }

    public void setTotalDistance(int totalDistance) {
        this.totalDistance = totalDistance;
    }

    public void setAvgDistance(double avgDistance) {
        this.avgDistance = avgDistance;
    }
    
    
    public DistanceStatEntry clone() {
       return (new DistanceStatEntry(this));
    }
    
    public String toString(){
        StringBuilder result = new StringBuilder();
        result.append("\nTotal distance: ");
        result.append(this.totalDistance);
        result.append("\nAverage distance per session: ");
        result.append(this.avgDistance);
        
        return super.toString() + result.toString();
    }
    
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        DistanceStatEntry dse = (DistanceStatEntry) o;
       
       return ( super.equals(o) && this.totalDistance == dse.getTotalDistance() && this.avgDistance == dse.getAvgDistance() );
   }
    
}
