
import java.io.Serializable;
import java.util.Arrays;
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
public class YearStat implements Serializable{
    private static final String[] months = {
    "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"};
    
    private MonthStat[] monthlyStats;
    
    public YearStat(){
        this.monthlyStats = new MonthStat[12];
    }
    
    public YearStat(MonthStat[] mStat, int year){
        this.monthlyStats = Arrays.copyOf(mStat, 12);
    }
    
    public YearStat(YearStat stats){
        this.monthlyStats = stats.getMonthlyStats();
    }
    
    public MonthStat[] getStats(){
        return Arrays.copyOf(this.monthlyStats, 12);
    }
    
    
    public void addStat(Activity act){
        int month = ( act.getDate().get(Calendar.MONTH) );
        if(monthlyStats[month] == null) this.addNewStat(act, month);
        else this.updateStat(act, month);
    }
    
    public void addNewStat(Activity act, int month){
        monthlyStats[month] = new MonthStat(act);
    }
    
    public void updateStat(Activity act, int month){
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
        this.monthlyStats = Arrays.copyOf(stats, 12);
    }
    
    public String showMonthlyStats(int month) throws StatsNotAvailableException{
        if(this.monthlyStats[month-1] == null)
            throw new StatsNotAvailableException("There are no statistics for that month");
        return this.monthlyStats[month-1].toString();
    }
    
    public long getTotalDuration(String act, int month){
        return monthlyStats[month].getTotalDuration(act);
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
        StringBuilder result = new StringBuilder();
        
        for(int i = 0; i<12; i++){
            if(monthlyStats[i] != null){
                result.append("\n-----Month: "+YearStat.months[i]+"-----\n");
                result.append(monthlyStats[i]+"\n");
            }
        }
        return result.toString();
    }    
}
