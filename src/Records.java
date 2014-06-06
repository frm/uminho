import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

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
    private HashMap<String, RecordEntry> records;
    
    //constructors

    /**
     *
     */
        public Records()
    {this.records = new HashMap<String,RecordEntry>();}
    
    /**
     *
     * @param rec
     */
    public Records(HashMap<String, RecordEntry> rec)
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
            RecordEntry entry = this.records.get(name);
            entry.updateRecords(act);
            this.records.put(name, entry);
        }
        else{
            this.records.put(name, createNewRecordEntry(act) );
        }
    }
    
    private RecordEntry createNewRecordEntry(Activity act){
        RecordEntry result;
        if(act instanceof AltitudeActivity)
            result = new AltitudeRecordEntry(act);
        else if(act instanceof DistanceActivity)
            result = new DistanceRecordEntry(act);
        else
            result = new RecordEntry(act);
        
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
    
    //getters & setters

    /**
     *
     * @return
     */
        public HashMap<String,RecordEntry> getRecords(){
        return cloneRecords(this.records);
    }
    
    /**
     *
     * @param rec
     */
    public void setRecords(HashMap<String, RecordEntry> rec)
    {this.records = cloneRecords(rec);}
    
    //methods
    private HashMap<String, RecordEntry> cloneRecords(HashMap<String, RecordEntry> entries ) {
        HashMap<String, RecordEntry> result = new HashMap<String, RecordEntry>();
        for(RecordEntry rec: entries.values()){
            result.put(rec.getName(), rec);
        }
        return result;
    }
    
    /**
     *
     * @param s
     * @return
     */
    public RecordEntry getRecordEntry(String s) {
        RecordEntry re = this.records.get(s);
        /* http://stackoverflow.com/questions/9335515/how-do-i-use-getconstructorparams-newinstanceargs
        Remove Commented Constructor on distance record entry
        try {
            return re.getClass().getConstructor().newInstance(re);
        } catch (NoSuchMethodException ex) {
            Logger.getLogger(Records.class.getName()).log(Level.SEVERE, null, ex);
        } catch (SecurityException ex) {
            Logger.getLogger(Records.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InstantiationException ex) {
            Logger.getLogger(Records.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IllegalAccessException ex) {
            Logger.getLogger(Records.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IllegalArgumentException ex) {
            Logger.getLogger(Records.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InvocationTargetException ex) {
            Logger.getLogger(Records.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        return null; */
        
        /*
        System.out.println(re.getClass().getSimpleName());
        if(re instanceof AltitudeRecordEntry)
            return ( (AltitudeRecordEntry)re ).clone();
        else if (re instanceof DistanceRecordEntry)
            return ( (DistanceRecordEntry)re ).clone();
        else return re.clone();
                */
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
