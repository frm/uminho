import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author tiago
 */
public class Records implements Serializable{
    private HashMap<String, Milestones> records;
    
    //constructors

    /**
     *
     */
        public Records()
    {this.records = new HashMap<String,Milestones>();}
    
    /**
     *
     * @param rec
     */
    public Records(HashMap<String, Milestones> rec)
    {this.records = cloneRecords(rec);}
    
    /**
     *
     * @param rec
     */
    public Records(Records rec)
    {this.records = rec.getRecords();}
    
    /**
     *
     * @param act
     */
    public void addRecord(Activity act){
        String name = act.getName();
        if( this.records.containsKey(name)){
            Milestones entry = this.records.get(name);
            addData(entry, act);
            this.records.put(name, entry);
        }
        else{
            this.records.put(name, createNewMilestones(act) );
        }
    }
   
    private Milestones createNewMilestones(Activity act){
        Milestones result;
        if(act instanceof AltitudeActivity)
            result = new AltitudeMilestones( (AltitudeActivity) act);
        else if(act instanceof DistanceActivity)
            result = new DistanceMilestones( (DistanceActivity) act);
        else
            result = new Milestones(act);
    
        return result;
    }
    
    /**
     *
     * @param act
     */
    public void removeRecord(Activity act){
        removeRecord( act.getName() );
    }
    
    /**
     *
     * @param name
     */
    public void removeRecord(String name){
        this.records.remove(name);
    }
    
    /**
     *
     * @param entry
     * @param act
     */
    public void addData(Milestones entry, Activity act){
        if(entry instanceof AltitudeMilestones)
            ( (AltitudeMilestones) entry).addData( (AltitudeActivity) act);
        else if(entry instanceof DistanceMilestones)
            ( (DistanceMilestones) entry).addData( (DistanceActivity) act);
        else
            entry.addData(act);
    }
    
    //getters & setters

    /**
     *
     * @return
     */
        public HashMap<String,Milestones> getRecords(){
        return cloneRecords(this.records);
    }
    
    /**
     *
     * @param rec
     */
    public void setRecords(HashMap<String, Milestones> rec)
    {this.records = cloneRecords(rec);}
    
    //methods
    private HashMap<String, Milestones> cloneRecords(HashMap<String, Milestones> entries ) {
        HashMap<String, Milestones> result = new HashMap<String, Milestones>();
        for(Map.Entry<String, Milestones> entry : entries.entrySet() ){
            result.put( entry.getKey(), entry.getValue().clone() );
        }
        return result;
    }
    
    /**
     *
     * @param s
     * @return
     */
    public Milestones getRecordEntry(String s) {
        return this.records.get(s).clone();
    }
    
    /**
     *
     * @param type
     * @return
     */
    public long getKmTimeAprox(String type){
        return ((DistanceMilestones) records.get(type)).getKmTimeAprox();
    }
    
    /**
     *
     * @param type
     * @return
     */
    public int getMaxRecordDistance(String type){
        return ((DistanceMilestones) records.get(type)).getMaxRecordDistance();
    }
    
    //essentials
    public Records clone()
    {return new Records(this);}
    
    public boolean equals(Object o){
        if( this == o) return true;
        
        if( o == null || this.getClass() != o.getClass() ) return false;
        
        Records rec = (Records) o;
        return this.records.equals(rec.getRecords());
    }
    
    public String toString(){
        return this.records.toString();
    }
}
