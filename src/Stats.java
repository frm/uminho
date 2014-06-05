
import java.io.Serializable;
import java.util.Calendar;
import java.util.HashMap;

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
        for(YearStat stat: annualStats.values()){
            result.put(stat.getYear(), stat.clone());
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

    private void addNewYearStat(Activity act){
        YearStat anStat = new YearStat();
        int year = act.getDate().get(Calendar.YEAR);
        anStat.setYear(year);
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
            throw new StatsNotAvailableException("There are no statistics for " + year);

        return this.annualStats.get(year).toString();
    }

    public String showMonthlyStats(int year, int month) throws StatsNotAvailableException{
        if(this.annualStats.get(year) == null)
            throw new StatsNotAvailableException("There are no statistics for " + year);

        YearStat yt = this.annualStats.get(year);
        return yt.showMonthlyStats(month);
    }

    public Stats clone(){
        return new Stats(this);
    }


    public String toString(){
        return annualStats.toString();
    }

    public boolean equals(Object o){
        if( this == o) return true;

        if( o == null || this.getClass() != o.getClass() ) return false;

        Stats stats = (Stats) o;

        return this.annualStats.equals(stats.getAnnualStats());
    }


}
