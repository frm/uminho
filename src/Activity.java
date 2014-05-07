
import java.util.GregorianCalendar; 

public abstract class Activity
{
   GregorianCalendar date;
   GregorianCalendar duration;
   int calories;

   void setDate(GregorianCalendar date){this.date = (GregorianCalendar) date.clone();}
   void setDuration(GregorianCalendar duration){this.duration = (GregorianCalendar) duration.clone();}
   void setCalories(int calories){this.calories = calories;}

  GregorianCalendar getDate(){return (GregorianCalendar) this.date.clone();}
  GregorianCalendar getDuration(){return (GregorianCalendar) this.duration.clone();}
  int getCalories(){return this.calories;}

  
}