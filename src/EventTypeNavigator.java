
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
public class EventTypeNavigator extends Navigator<String>{
    private FitnessUM app;
    
    public EventTypeNavigator() {
        super();
    }
    
    public EventTypeNavigator(ArrayList<String> list, FitnessUM app ){
        super(list);
        this.app = app;
    }

    public FitnessUM getApp() {
        return app;
    }

    public void setApp(FitnessUM app) {
        this.app = app;
    }
    

    @Override
    public void print(String s) {
        System.out.println( s );
    }
    
    @Override
    public void select(String s){ 
        app.getEventInfo(s);
    }
    
    @Override
    public String emptyMessage() {
        return "\nThe app has no Activities available\n";
    }
}
