
import java.util.ArrayList;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author tiago
 */
public class RecordsNavigator extends Navigator<String>{
    private UserController uc;
    
    public RecordsNavigator() {
        super();
        this.uc = new UserController();
    }
    
    public RecordsNavigator(ArrayList<String> list) {
        super(list);
        this.uc = new UserController();
    }
    
    public RecordsNavigator(ArrayList<String> list, UserController userc ){
        super(list);
        this.uc = userc.clone();
    }

    public void print(String s) {
        System.out.println( s );
    }
    
    public void select(String s){ 
        RecordEntry re = this.uc.getRecordEntry(s);
        System.out.println( re );
    }
    
    public String emptyMessage() {
        return "\nUser has no activities\n";
    }
}
