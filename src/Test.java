/**
 *
 * @author frmendes
 */
import java.util.GregorianCalendar;

public class Test {
    
    /*
    public static void main(String[] args) {
        Test.userTest();
        //Test.userInfoTest();
        Test.activityLogTest();
    }*/
    
    public static void userTest() {
        System.out.println("Testing empty constructor - Should be empty");
        User empty = new User();
        System.out.println(empty);
        
        UserList friendo = new UserList();
        UserInfo info = new UserInfo();
        Records rec = new Records();
        System.out.println("Testing normal constructor - Myself");
        User mendes = new User("Fernando Mendes", "ilol", "thisnthat@overthe.re", friendo, info, rec);
        System.out.println(mendes);
        
        System.out.println("Testing copy constructor - Myself");
        System.out.println(new User(mendes));
        
        System.out.println("Testing clone - Myself");
        System.out.println( mendes.clone() );
        
        if( new User(mendes).equals(mendes) && !empty.equals(mendes) )
            System.out.println("USER EQUALS WORKS");
        else
            System.out.println("BUG ON USER EQUALS");
        
        System.out.println("Testing getters and setters - Myself");
        empty.setName( mendes.getName() );
        empty.setEmail( mendes.getEmail() );
        empty.setId( mendes.getId() );
        empty.setFriends(mendes.getFriends());
        empty.setInfo(mendes.getInfo());
        empty.setRecords(mendes.getRecords());
        System.out.println( empty + "\nWITH ID: " + empty.getId() );
        
        System.out.println("Testing db");
        System.out.println("Testing empty constructor: " + new UserDatabase() );
        
        System.out.println("Testing copy constructor");
        UserDatabase my_db = new UserDatabase();
        my_db.save(mendes);
        my_db.save(new User("A", "BB", "CCC", friendo, info, rec) );
        System.out.println(my_db);
        System.out.println(my_db.clone());
        
        System.out.println("Testing findById");
        System.out.println(my_db.findById(1));
        
        UserDatabase empty_db = new UserDatabase();
        
        if (my_db.equals( my_db.clone() ) && !my_db.equals(empty) )
            System.out.println("USER DATABASE EQUALS WORKS");
        else
            System.out.println("BUG IN USER DATABASE EQUALS");
        
       System.out.println("Testing change to user settings");
       mendes = my_db.findById(1);
       mendes.setName("Not Mendes");
       my_db.save(mendes);
       System.out.println(my_db);
        
    }
    public static void userInfoTest(){
        System.out.println("Testing empty constructor - Should be empty");
        UserInfo empty = new UserInfo();
        System.out.println(empty);
        
        System.out.println("Testing normal constructor - Myself");
        UserInfo mendes = new UserInfo(false, 170.0, 65.0,new GregorianCalendar(), "Sitting");
        System.out.println(mendes);
        
        System.out.println("Testing copy constructor - Myself");
        System.out.println(new UserInfo(mendes));
        
        System.out.println("Testing clone - Myself");
        System.out.println( mendes.clone() );
        
        if( new UserInfo(mendes).equals(mendes) && !empty.equals(mendes) )
            System.out.println("USER INFO EQUALS WORKS");
        else
            System.out.println("BUG ON USER INFO EQUALS");
        
        System.out.println("Testing getters and setters - Myself");
        empty.setGender( mendes.getGender() );
        empty.setHeight( mendes.getHeight() );
        empty.setWeight( mendes.getWeight() );
        empty.setBirthDate( mendes.getBirthDate() );
        empty.setFavoriteSport( mendes.getFavoriteSport() );
        System.out.println( empty );
    }
    
    public static void activityLogTest(){
        System.out.println("Testing empty constructor - Should be empty");
        ActivityLog empty = new ActivityLog();
        System.out.println(empty);
        
        Activity act = new Activity("Andar", 2, new GregorianCalendar(), new GregorianCalendar(), 4000);
        
        empty.addActivity(act);
        
        System.out.println(empty);
        
    }
}