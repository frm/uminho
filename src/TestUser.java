/**
 *
 * @author frmendes
 */

public class TestUser {
    
    public static void main(String[] args) {
        TestUser.test();
    }
    
    public static void test() {
        System.out.println("Testing empty constructor - Should be empty");
        User empty = new User();
        System.out.println(empty);
        
        System.out.println("Testing normal constructor - Myself");
        User mendes = new User("Fernando Mendes", "ilol", "thisnthat@overthe.re");
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
        System.out.println( empty + "\nWITH ID: " + empty.getId() );
        
        System.out.println("Testing db");
        System.out.println("Testing empty constructor: " + new UserDatabase() );
        
        System.out.println("Testing copy constructor");
        UserDatabase my_db = new UserDatabase();
        my_db.save(mendes);
        my_db.save(new User("A", "BB", "CCC") );
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
}
