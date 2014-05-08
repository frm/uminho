
import java.io.Serializable;
import java.util.Comparator;
/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author joaorodrigues
 */
class ActivityComparator implements Comparator<Activity>, Serializable {
    
    @Override
    public int compare(Activity a1, Activity a2){
        int compare = (a1.getDate()).compareTo(a2.getDate());
        if( compare < 0) return 1;
        if( compare > 0) return -1;
        else return 0;
        
   }
}
