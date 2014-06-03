/**
 *
 * @author tiago
 */
public class RecordEntry {
    private String name;
    private int nrEntries;
    private long maxDuration;
    private int maxCalories;
    private Achievements achievList;
    
    public RecordEntry(){
        this.name = "";
        this.nrEntries = 0;
        this.maxDuration = 0;
        this.maxCalories = 0;
        this.achievList.populateAchievs();
    }
    
    public RecordEntry(String n, int nE, long mD, int mC){
        this.name = n;
        this.nrEntries = nE;
        this.maxDuration = mD;
        this.maxCalories = mC;
        this.achievList.populateAchievs();
    }
    
    public RecordEntry(String n, int nE, long mD, int mC, Achievements ach){
        this.name = n;
        this.nrEntries = nE;
        this.maxDuration = mD;
        this.maxCalories = mC;
        this.achievList = ach;
    }
    
    public RecordEntry(RecordEntry re){
        this.name = re.getName();
        this.nrEntries = re.getNrEntries();
        this.maxDuration = re.getMaxDuration();
        this.maxCalories = re.getMaxCalories();
        this.achievList = re.getAchievList();
    }
    
    //getters
    public String getName()
    {return this.name;}
    
    public int getNrEntries()
    {return this.nrEntries;}
    
    public long getMaxDuration()
    {return this.maxDuration;}
    
    public int getMaxCalories()
    {return this.maxCalories;}
    
    public Achievements getAchievList()
    {return new Achievements(this.achievList);}

    //setters
    public void setName(String n)
    {this.name = n;}
    
    public void setNrEntries(int ne)
    {this.nrEntries = ne;}
    
    public void setMaxDuration(long md)
    {this.maxDuration = md;}
    
    public void setMaxCalories(int c)
    {this.maxCalories = c;}
        
    public void setAchievList(Achievements a)
    {this.achievList = new Achievements(this.achievList);}
    
    //essentials
    public RecordEntry clone()
    {return new RecordEntry(this);}
    
    public boolean equals(Object obj){
        if (obj == this) {return true;}
        
        if (obj == null || getClass() != obj.getClass()) {return false;}
        
        RecordEntry re = (RecordEntry) obj;
        return (this.name.equals(re.getName()) && this.nrEntries == re.getNrEntries() && this.maxDuration == re.getMaxDuration() && this.maxCalories == re.getMaxCalories() && this.achievList.equals(re.getAchievList()));
    }
}