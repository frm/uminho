/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers;

/**
 *
 * @author mendes
 */
public class ControllerFactory {
    private static VolunteersController volunteersController;
    private static ActivityController activityController;
    private static RepresentativesController representativesController;
    
    public static VolunteersController getVolunteersController() {
        if (volunteersController == null)
            volunteersController = new VolunteersController();
        
        return volunteersController;
    }
    
    public static ActivityController getActivityController() {
        if (activityController == null)
            activityController = new ActivityController();
        
        return activityController;
    }
    
    public static RepresentativesController getRepresentativesController() {
        if (representativesController == null)
            representativesController = new RepresentativesController();
        
        return representativesController;
    }

}
