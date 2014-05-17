
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
public class AddActivityNavigator extends Navigator<String>{
    private int category;
    private FitnessUM app;
    
    public AddActivityNavigator() {
        super();
        this.app = new FitnessUM();
        this.category = 0;
    }
    
    public AddActivityNavigator(ArrayList<String> list) {
        super(list);
        this.app = new FitnessUM();
        this.category = 0;
    }
    
    public AddActivityNavigator(int category, FitnessUM app, ArrayList<String> list ){
        super(list);
        this.app = app;
        this.category = category;
    }
    
    public void print(String name) {
        System.out.println( name );
    }
    
    public void select(String name){
        this.app.addActivitySession(category, name);
    }
    
    public String emptyMessage() {
        return "\nUser has no activities\n";
    }
}
