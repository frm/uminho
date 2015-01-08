/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers;

import data.RepositoryFactory;

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
    private static TasksController taskController;
    private static ProjectsController projectController;
    private static PaymentPlanController paymentPlanController;
    private static PaymentsController paymentController;
    private static DonorsController donorsController;
    private static EventsController eventsController;
    private static MembersController membersController;
    private static ApplicationsController applicationsController;
    private static EmployeeController employeeController;
    private static QuestionsController questionsController;

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

    public static TasksController getTaskController() {
        if (taskController == null)
            taskController = new TasksController();

        return taskController;
    }

    public static ProjectsController getProjectController() {
        if (projectController == null)
            projectController = new ProjectsController();

        return projectController;
    }

    public static PaymentPlanController getPaymentPlanController() {
        if (paymentPlanController == null)
            paymentPlanController = new PaymentPlanController();

        return paymentPlanController;
    }

    public static PaymentsController getPaymentController() {
        if (paymentController == null)
            paymentController = new PaymentsController();

        return paymentController;
    }

    public static DonorsController getDonorsController() {
        if (donorsController == null)
            donorsController = new DonorsController();

        return donorsController;
    }

    public static EventsController getEventsController() {
        if (eventsController == null)
            eventsController = new EventsController();

        return eventsController;
    }
    public static MembersController getMembersController() {
        if (membersController == null)
            membersController = new MembersController();

        return membersController;
    }

    public static ApplicationsController getApplicationsController() {
        if (applicationsController == null)
            applicationsController = new ApplicationsController();

        return applicationsController;
    }
    
    public static EmployeeController getEmployeeController() {
        if (employeeController == null)
            employeeController = new EmployeeController();

        return employeeController;
    }
    
    public static QuestionsController getQuestionsController() {
        if (questionsController == null)
            questionsController = new QuestionsController();

        return questionsController;
    }
    
    public static boolean init(String username, String password) {
        return RepositoryFactory.init(username, password);
    }
}
