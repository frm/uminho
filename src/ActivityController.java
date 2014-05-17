
import java.util.ArrayList;
/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author joaorodrigues
 */
public class ActivityController {
    private ArrayList<String> simpleActivities;
    private ArrayList<String> distanceActivities;
    private ArrayList<String> altitudeActivities;

    public ActivityController() {
        this.simpleActivities = new ArrayList<String>();
        this.distanceActivities = new ArrayList<String>();
        this.altitudeActivities = new ArrayList<String>();
    }

    public ActivityController(ArrayList<String> simpleActivities, ArrayList<String> distanceActivities, ArrayList<String> altitudeActivities) {
        this.simpleActivities = (ArrayList<String>) simpleActivities.clone();
        this.distanceActivities = (ArrayList<String>) distanceActivities.clone();
        this.altitudeActivities = (ArrayList<String>) altitudeActivities.clone();
    }
    
    public ActivityController(ActivityController ac){
        this.simpleActivities = ac.getSimpleActivities();
        this.distanceActivities = ac.getDistanceActivities();
        this.altitudeActivities = ac.getAltitudeActivities();
    }

    public ArrayList<String> getSimpleActivities() {
        return (ArrayList<String>) this.simpleActivities.clone();
    }

    public ArrayList<String> getDistanceActivities() {
        return (ArrayList<String>) this.distanceActivities.clone();
    }

    public ArrayList<String> getAltitudeActivities() {
        return (ArrayList<String>) this.altitudeActivities.clone();
    }

    public void setSimpleActivities(ArrayList<String> simpleActivities) {
        this.simpleActivities = (ArrayList<String>)simpleActivities.clone();
    }

    public void setDistanceActivities(ArrayList<String> distanceActivities) {
        this.distanceActivities = (ArrayList<String>)distanceActivities.clone();
    }

    public void setAltitudeActivities(ArrayList<String> altitudeActivities) {
        this.altitudeActivities = (ArrayList<String>)altitudeActivities.clone();
    }
    
    public void setActivities(ArrayList<String> simple, ArrayList<String> distance, ArrayList<String> altitude){
        this.setSimpleActivities(simple);
        this.setDistanceActivities(distance);
        this.setAltitudeActivities(altitude);
    }
    
    public void addNewActivity(int type, String name){
        switch (type) {
            case 1:  this.simpleActivities.add(name);
                     break;
            case 2:  this.distanceActivities.add(name);
                     break;
            case 3:  this.altitudeActivities.add(name);
                     break;
        }
        
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        result.append("### Activity Controller: ###");
        result.append("\nSimple Activities: ");
        result.append(this.simpleActivities);
        result.append("\nDistance Activities: ");
        result.append(this.distanceActivities);
        result.append("\nAltitude Activities: ");
        result.append(this.altitudeActivities);

        return result.toString();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
       
        ActivityController ac = (ActivityController) obj;
        
        return ( this.simpleActivities.equals(ac.getSimpleActivities()) && this.distanceActivities.equals(ac.getDistanceActivities()) && this.altitudeActivities.equals(ac.getAltitudeActivities()));
    }
    
    @Override
    public ActivityController clone(){
       return new ActivityController(this);
    }
}
