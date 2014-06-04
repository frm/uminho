import java.util.HashMap;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author tiago
 */
public class Records {
    private HashMap<String, RecordEntry> records;
    
    //constructors
    public Records()
    {this.records = new HashMap<String,RecordEntry>();}
    
    public Records(HashMap<String, RecordEntry> rec)
    {this.records = cloneRecords(rec);}
    
    public Records(Records rec)
    {this.records = rec.getRecords();}
    
  
    public void addRecords(Activity act){
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
    
    //getters & setters
    public HashMap<String,RecordEntry> getRecords()
    {return cloneRecords(this.records);}
    
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
