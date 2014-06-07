
import java.util.ArrayList;
import java.util.TreeMap;
import java.util.TreeSet;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author frmendes
 */
public class SimulationNavigator extends Navigator<TreeMap<Long, User>> {

    public SimulationNavigator() {
        super();
    }

    public SimulationNavigator(ArrayList<TreeMap<Long, User>> list) {
        super(list);
    }

    public void print(TreeMap<Long, User> km) { }

    public void select(TreeMap<Long, User> km) {
        Event.printSimulatedKm(km);
    }

    public String emptyMessage() {
        return "\nNo kilometers available\n";
    }
}
