
import java.util.ArrayList;
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
public class EventSimulation {
    private ArrayList< TreeSet<Participant> > kms;
    
    public EventSimulation() {
        this.kms = new ArrayList< TreeSet<Participant> >();
    }
    
    public EventSimulation(ArrayList<TreeSet<Participant>> sim) {
        this.kms = EventSimulation.cloneSimulation(sim);
    }
    
    public EventSimulation(EventSimulation es) {
        this.kms = es.getSimulation();
    }
    
    public EventSimulation(ArrayList<User> signups) {
        this.kms.add(new TreeSet)
        for(User u : signups)
    }
    
    public void setSimulation(ArrayList<TreeSet<Participant>> sim) {
        this.kms = EventSimulation.cloneSimulation(sim);
    }
    
    public ArrayList< TreeSet<Participant> > getSimulation() {
        return EventSimulation.cloneSimulation(this.kms);
    }
    
    public void simulate( ArrayList<User> signups ) {
        for(User u : signups)
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for(int i = 0; i < this.kms.size(); i++) {
            sb.append("### KM " + i + "###\n");
            for(Participant p : this.kms.get(i) ) {
                int j = 1;
                sb.append("\t" + j++ + ": ");
                sb.append("\t Name: ");
                sb.append("\t" + p.getName() );
                sb.append("\t Time: ");
                if( p.forfeit() )
                    sb.append("\t + FORFEIT");
                else
                    sb.append("\t" + p.getTime() );
            }
        }
        return sb.toString();
    }
    
    @Override
    public EventSimulation clone() {
        return new EventSimulation(this);
    }
    
    public boolean equals(Object o) {
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass()) return false;

        EventSimulation e = (EventSimulation) o;
        
        return ( this.kms.equals( e.getSimulation() ) );
    }
    
    private static ArrayList<TreeSet<Participant>> cloneSimulation(ArrayList<TreeSet<Participant>> sim) {
        ArrayList<TreeSet<Participant>> newSim = new ArrayList<TreeSet<Participant>>();
        
        for(TreeSet<Participant> t : sim) {
            TreeSet<Participant> newTree = new TreeSet<Participant>();
            
            for(Partipant p : t)
                newTree.add( p.clone() );
            
            newSim.add(newTree);
        }
        
        return newSim;
    }
    
}
