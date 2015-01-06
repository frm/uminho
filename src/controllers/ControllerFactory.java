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
    private static ContactsController contactsController;
    private static FamiliesController familiesController;
    private static TaskController taskController;
    private static ProjectController projectController;
    private static PaymentPlanController paymentPlanController;
    private static PaymentController paymentController;
    private static DonorsController donorsController;  
    
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

    public static ContactsController getContactsController() {
        if (contactsController == null)
            contactsController = new ContactsController();

        return contactsController;
    }
    public static FamiliesController getFamiliesController() {
        if (familiesController == null)
            familiesController = new FamiliesController();

        return familiesController;
    }
    
    public static TaskController getTaskController() {
        if (taskController == null)
            taskController = new TaskController();
        
        return taskController;
    }
    
    public static ProjectController getProjectController() {
        if (projectController == null)
            projectController = new ProjectController();
        
        return projectController;
    }
    
    public static PaymentPlanController getPaymentPlanController() {
        if (paymentPlanController == null)
            paymentPlanController = new PaymentPlanController();
        
        return paymentPlanController;
    }
    
    public static PaymentController getPaymentController() {
        if (paymentController == null)
            paymentController = new PaymentController();
        
        return paymentController;
    }

    public static DonorsController getDonorsController() {
        if (donorsController == null)
            donorsController = new DonorsController();
        
        return donorsController;
    }
}
