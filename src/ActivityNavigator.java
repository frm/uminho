
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
public abstract class ActivityNavigator extends Navigator<String>{
    private int category;
    private FitnessUM app;
    
    public ActivityNavigator() {
        super();
        this.app = new FitnessUM();
        this.category = 0;
    }
    
    public ActivityNavigator(ArrayList<String> list) {
        super(list);
        this.app = new FitnessUM();
        this.category = 0;
    }
    
    public ActivityNavigator(int category, FitnessUM app, ArrayList<String> list ){
        super(list);
        this.app = app;
        this.category = category;
    }
    
    public void print(String name) {
        System.out.println( name );
    }
    
    public abstract void select(String nome);

    public int getCategory() {
        return category;
    }

    public FitnessUM getApp() {
        return app.clone();
    }

    public void setCategory(int category) {
        this.category = category;
    }

    public void setApp(FitnessUM app) {
        this.app = app.clone();
    }
    
    
    
    public String emptyMessage() {
        return "\nUser has no activities\n";
    }
}
