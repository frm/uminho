
import java.util.Calendar;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author joaorodrigues
 */
public class YearStat {
    private MonthStat[] monthlyStats;
    private int year;
    
    public YearStat(){
        this.monthlyStats = new MonthStat[12];
        this.year = 0;
    }
    
    public YearStat(MonthStat[] mStat, int year){
        this.monthlyStats = mStat.clone();
        this.year = year;
    }
    
    public YearStat(YearStat stats){
        this.monthlyStats = stats.getMonthlyStats();
        this.year = stats.getYear();
    }
    
    public MonthStat[] getStats(){
        return this.monthlyStats.clone();
    }
    
    public int getYear(){
        return this.year;
    }
    
    public void setYear(int year){
        this.year = year;
    }
    
    public void addStat(Activity act){
        int month = ( act.getDate().get(Calendar.MONTH) );
        monthlyStats[month].addStat(act);
    }
    
    public boolean removeActivityStat(Activity act){
        int month = ( act.getDate().get(Calendar.MONTH) );
        return monthlyStats[month].removeStat(act);
    }

    public MonthStat[] getMonthlyStats() {
        return this.monthlyStats.clone();
    }

    public void setStats(MonthStat[] stats) {
        this.monthlyStats = stats.clone();
    }
    
    public String showMonthlyStats(int month) throws StatsNotAvailable{
        if(this.monthlyStats[month-1] == null)
            throw new StatsNotAvailable("There are no statistics for that month");
        return this.monthlyStats[month-1].toString();
    }
    
    public YearStat clone(){
        YearStat stats = new YearStat();
        
        try {
           stats = new YearStat(this);
            
        } catch (NullPointerException e) {
            System.out.println("No stats yet");
        }
        
        return stats;
    }
    
    public boolean equals(Object o){
        if( this == o) return true;
        
        if( o == null || this.getClass() != o.getClass() ) return false;
        
        YearStat stats = (YearStat) o;
        
        return this.monthlyStats.equals(stats.getStats() );
    }
    
    public String toString(){
        return (this.monthlyStats.toString() );
    }    
}
