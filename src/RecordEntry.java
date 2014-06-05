
import java.io.Serializable;

/**
 *
 * @author tiago
 */
public class RecordEntry implements Serializable{
    private String name;
    private Milestones milestones;

    
    public RecordEntry(){
        this.name = "";
        this.milestones = new Milestones();

    }
    
    public RecordEntry(Activity act){
        this.name = act.getName();
        this.milestones = new Milestones();
        this.milestones.addData(act);

    }
    
    public RecordEntry(String n, Milestones ms){
        this.name = n;
        this.milestones = ms.clone();

    }
    
    
    public RecordEntry(RecordEntry re){
        this.name = re.getName();
        this.milestones = re.getMilestones();
    }
    
    
    public void updateRecords(Activity act){
        this.milestones.addData(act);
    }
    //getters
    public String getName()
    {return this.name;}

    public Milestones getMilestones() {
        return milestones.clone();
    }
    

    //setters
    public void setName(String n)
    {this.name = n;}

    public void setMilestones(Milestones milestones) {
        this.milestones = milestones.clone();
    }
    

    //essentials
    public RecordEntry clone()
    {return new RecordEntry(this);}
    
    public boolean equals(Object obj){
        if (obj == this) {return true;}
        
        if (obj == null || getClass() != obj.getClass()) {return false;}
        
        RecordEntry re = (RecordEntry) obj;
        return (this.name.equals(re.getName()) );
    }
    
    public String toString(){
        return ( "Basic Milestones: \n" + this.milestones.toString() );
    }
}