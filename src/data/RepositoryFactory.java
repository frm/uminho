/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package data;

/**
 *
 * @author frmendes
 */
public class RepositoryFactory {

    private static VolunteersRepository volunteersRepository;
    private static ActivityRepository activityRepository;
    private static RepresentativeRepository representativeRepository;
    private static ContactRepository contactRepository;
    private static FamilyRepository familyRepository;
    private static PaymentRepository paymentRepository;
    private static PaymentPlanRepository paymentPlanRepository;
    private static ProjectRepository projectRepository;
    private static TaskRepository taskRepository;
    private static DonorRepository donorRepository;
    private static EventsRepository eventsRepository;
    private static MemberRepository memberRepository;
    private static ApplicationRepository applicationRepository;
    private static EmployeeRepository employeeRepository;
    private static QuestionRepository questionRepository;

    private static String USERNAME;
    private static String PASSWORD;
    private static final String URL = "jdbc:mysql://localhost/habitat";

    public RepositoryFactory() {
        volunteersRepository = new VolunteersRepository(getURL(), USERNAME, PASSWORD);
    }

    // TODO change this to generate valid mysql db url
    public static String getURL() {
        return URL;
    }

    public static VolunteersRepository getVolunteersRepository() {
        if (volunteersRepository == null)
            volunteersRepository = new VolunteersRepository(getURL(), USERNAME, PASSWORD);

        return volunteersRepository;
    }

    public static ActivityRepository getActivityRepository() {
        if (activityRepository == null)
            activityRepository = new ActivityRepository(getURL(), USERNAME, PASSWORD);

        return activityRepository;
    }

    public static RepresentativeRepository getRepresentativeRepository() {
        if (representativeRepository == null)
            representativeRepository = new RepresentativeRepository(getURL(), USERNAME, PASSWORD);

        return representativeRepository;
    }

    public static ContactRepository getContactRepository() {
        if (contactRepository == null)
            contactRepository = new ContactRepository(getURL(), USERNAME, PASSWORD);

        return contactRepository;
    }

    public static FamilyRepository getFamilyRepository() {
        if (familyRepository == null)
            familyRepository = new FamilyRepository(getURL(), USERNAME, PASSWORD);

        return familyRepository;   
    }
    
    
    public static PaymentRepository getPaymentRepository() {
        if (paymentRepository == null)
            paymentRepository = new PaymentRepository(getURL(), USERNAME, PASSWORD);
        
        return paymentRepository;
    }
    
    public static PaymentPlanRepository getPaymentPlanRepository() {
        if (paymentPlanRepository == null)
            paymentPlanRepository = new PaymentPlanRepository(getURL(), USERNAME, PASSWORD);
        
        return paymentPlanRepository;
    }
    
    public static ProjectRepository getProjectRepository() {
        if (projectRepository == null)
            projectRepository = new ProjectRepository(getURL(), USERNAME, PASSWORD);
        
        return projectRepository;
    }
    
    public static TaskRepository getTaskRepository() {
        if (taskRepository == null)
            taskRepository = new TaskRepository(getURL(), USERNAME, PASSWORD);
        
        return taskRepository;
    }
    
    public static DonorRepository getDonorRepository() {
        if (donorRepository == null)
            donorRepository = new DonorRepository(getURL(), USERNAME, PASSWORD);
        
        return donorRepository;
    }
    
    public static EventsRepository getEventsRepository() {
        if (eventsRepository == null)
            eventsRepository = new EventsRepository(getURL(), USERNAME, PASSWORD);
        
        return eventsRepository;
    }
    
    public static ApplicationRepository getApplicationRepository() {
        if (applicationRepository == null)
            applicationRepository = new ApplicationRepository(getURL(), USERNAME, PASSWORD);
        
        return applicationRepository;
    }
/*
    public static VolunteersEventRepository getVolunteersEventRepository() {
        if (volunteersEventRepository == null)
            volunteersEventRepository = new VolunteersEventRepository(getURL(), USERNAME, PASSWORD);
        
        return volunteersEventRepository;
    }
*/
    public static MemberRepository getMemberRepository() {
        if (memberRepository == null)
            memberRepository = new MemberRepository(getURL(), USERNAME, PASSWORD);

        return memberRepository;
    }
    
    public static EmployeeRepository getEmployeeRepository() {
        if (employeeRepository == null)
            employeeRepository = new EmployeeRepository(getURL(), USERNAME, PASSWORD);

        return employeeRepository;
    }
    
    public static QuestionRepository getQuestionRepository() {
        if (questionRepository == null)
            questionRepository = new QuestionRepository(getURL(), USERNAME, PASSWORD);

        return questionRepository;
    }
    
    public static boolean init(String username, String password) {
        RepositoryFactory.USERNAME = username;
        RepositoryFactory.PASSWORD = password;
        return new QuestionRepository(getURL(), USERNAME, PASSWORD).attemptLogin();
    }
}
