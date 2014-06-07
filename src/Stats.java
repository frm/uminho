
import java.io.Serializable;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author joaorodrigues
 */
public class Stats implements Serializable{
    private HashMap<Integer, YearStat> annualStats;

    public Stats(){
        this.annualStats = new HashMap<Integer, YearStat>();
    }

    public Stats(HashMap<Integer, YearStat> annualStats){
        this.annualStats = cloneAnnualStats(annualStats);
    }

    public Stats(Stats s){
        this.annualStats = s.getAnnualStats();
    }

    private HashMap<Integer, YearStat> cloneAnnualStats(HashMap<Integer, YearStat> annualStats) {
        HashMap<Integer, YearStat> result = new HashMap<Integer, YearStat>();
        for(Map.Entry<Integer, YearStat> pair: annualStats.entrySet()){
            result.put(pair.getKey(), pair.getValue().clone());
        }
        return result;
    }

    public HashMap<Integer, YearStat> getAnnualStats(){
        return cloneAnnualStats(this.annualStats);
    }

    public void setAnnualStats(HashMap<Integer, YearStat> annualStats){
        this.annualStats = cloneAnnualStats(annualStats);
    }

    public void addStat(Activity act){
        int year = act.getDate().get(Calendar.YEAR);
        if(this.annualStats.containsKey(year)){
            updateYearStat(act);
        }
        else addNewYearStat(act);

    }
    
    public boolean isEmpty(){
        return this.annualStats.isEmpty();
    }

    private void addNewYearStat(Activity act){
        YearStat anStat = new YearStat();
        int year = act.getDate().get(Calendar.YEAR);
        anStat.addStat(act);
        annualStats.put(year, anStat);
    }

    private void updateYearStat(Activity act){
        int year = act.getDate().get(Calendar.YEAR);
        YearStat ys = this.annualStats.get(year);
        ys.addStat(act);
        this.annualStats.put(year, ys);
    }

    public boolean removeActivityStat(Activity act){
        int year = act.getDate().get(Calendar.YEAR);
        YearStat ys = this.annualStats.get(year);
        boolean result = ys.removeActivityStat(act);
        this.annualStats.put(year, ys);

        return result;
    }


    public String showAnnualStats(int year) throws StatsNotAvailableException{
        if(this.annualStats.get(year) == null)
            throw new StatsNotAvailableException();

        return this.annualStats.get(year).toString();
    }

    public String showMonthlyStats(int year, int month) throws StatsNotAvailableException{
        if(this.annualStats.get(year) == null)
            throw new StatsNotAvailableException();

        YearStat yt = this.annualStats.get(year);
        return yt.showMonthlyStats(month);
    }

    public long getTotalDuration(String act, int year ,int month){
            return annualStats.get(year).getTotalDuration(act,month);
    }
                    
    public Stats clone(){
        return new Stats(this);
    }


    public String toString(){
        StringBuilder result = new StringBuilder();
        for(Map.Entry<Integer, YearStat> pair : this.annualStats.entrySet()){
            result.append("\n________Year: " + pair.getKey()+"________\n");
            result.append( pair.getValue() + "\n");
        }
        
        return result.toString();
    }

    public boolean equals(Object o){
        if( this == o) return true;

        if( o == null || this.getClass() != o.getClass() ) return false;

        Stats stats = (Stats) o;

        return this.annualStats.equals(stats.getAnnualStats());
    }


}
