
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
public class EventTypeUserNavigator extends EventTypeNavigator{

    public EventTypeUserNavigator() {
        super();
    }
    
    public EventTypeUserNavigator(ArrayList<String> list, FitnessUM app ){
        super(list, app);
    }

    
    
    @Override
    public void select(String s){ 
        super.getApp().listUpcomingEvents(s);
    }
}
