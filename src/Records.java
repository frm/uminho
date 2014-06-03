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
    private HashMap<Integer, RecordEntry> records;
    
    //constructors
    public Records()
    {this.records = new HashMap<Integer,RecordEntry>();}
    
    public Records(HashMap<Integer, RecordEntry> rec)
    {this.records = cloneRecords(rec);}
    
    public Records(Records rec)
    {this.records = rec.getRecords();}
    
    //getters & setters
    public HashMap<Integer,RecordEntry> getRecords()
    {return cloneRecords(this.records);}
    
    public void setRecords(HashMap<Integer, RecordEntry> rec)
    {this.records = cloneRecords(rec);}
    
    //methods
    public HashMap<Integer, RecordEntry> cloneRecords(HashMap<Integer, RecordEntry> rec){
        HashMap<Integer, RecordEntry> aux = new HashMap<Integer,RecordEntry>();
        aux.putAll(rec);
        return aux;
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
