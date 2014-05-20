
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
public class StatsNavigator extends ActivityNavigator{
    public StatsNavigator() {
        super();
    }
    
    public StatsNavigator(ArrayList<String> list) {
        super(list);
    }
    
    public StatsNavigator(int category, FitnessUM app, ArrayList<String> list ){
        super(category, app, list);
    }
    
    public void select(String name){
        this.getApp().showStatsByName(name);
    }
}
