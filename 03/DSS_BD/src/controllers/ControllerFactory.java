/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers;

import data.RepositoryFactory;
import models.Activity;
import models.Application;
import models.Contact;
import models.Donor;
import models.Employee;
import models.Event;
import models.Family;
import models.Payment;
import models.PaymentPlan;
import models.Project;
import models.Question;
import models.Representative;
import models.SimpleMember;
import models.Task;

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

    public static Controller getVolunteersController() {
        if (volunteersController == null)
            volunteersController = new VolunteersController();

        return volunteersController;
    }

    public static Controller<Activity> getActivityController() {
        if (activityController == null)
            activityController = new ActivityController();

        return activityController;
    }

    public static Controller<Representative> getRepresentativesController() {
        if (representativesController == null)
            representativesController = new RepresentativesController();

        return representativesController;
    }

    public static Controller<Contact> getContactsController() {
        if (contactsController == null)
            contactsController = new ContactsController();

        return contactsController;
    }
    public static Controller<Family> getFamiliesController() {
        if (familiesController == null)
            familiesController = new FamiliesController();

        return familiesController;
    }

    public static Controller<Task> getTaskController() {
        if (taskController == null)
            taskController = new TasksController();

        return taskController;
    }

    public static Controller<Project> getProjectController() {
        if (projectController == null)
            projectController = new ProjectsController();

        return projectController;
    }

    public static Controller<PaymentPlan> getPaymentPlanController() {
        if (paymentPlanController == null)
            paymentPlanController = new PaymentPlanController();

        return paymentPlanController;
    }

    public static Controller<Payment> getPaymentController() {
        if (paymentController == null)
            paymentController = new PaymentsController();

        return paymentController;
    }

    public static Controller<Donor> getDonorsController() {
        if (donorsController == null)
            donorsController = new DonorsController();

        return donorsController;
    }

    public static Controller<Event> getEventsController() {
        if (eventsController == null)
            eventsController = new EventsController();

        return eventsController;
    }
    public static Controller<SimpleMember> getMembersController() {
        if (membersController == null)
            membersController = new MembersController();

        return membersController;
    }

    public static Controller<Application> getApplicationsController() {
        if (applicationsController == null)
            applicationsController = new ApplicationsController();

        return applicationsController;
    }
    
    public static Controller<Employee> getEmployeeController() {
        if (employeeController == null)
            employeeController = new EmployeeController();

        return employeeController;
    }
    
    public static Controller<Question> getQuestionsController() {
        if (questionsController == null)
            questionsController = new QuestionsController();

        return questionsController;
    }
    
    public static boolean init(String username, String password) {
        return RepositoryFactory.init(username, password);
    }
}
