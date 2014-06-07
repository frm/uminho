
import java.io.Serializable;
import java.util.Comparator;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author frmendes
 */
public class SimulationComparator implements Comparator<Long>, Serializable {
    @Override
    public int compare(Long sim1, Long sim2) {
        if(sim1 < sim2) return 1;
        if(sim1 > sim2) return -1;
        else {
            int rand1 = (int)(Math.random() * 100);
            int rand2 = (int)(Math.random() * 100);
            
            while(rand1 == rand2) {
                rand1 = (int)(Math.random() * 100);
                rand2 = (int)(Math.random() * 100);
            }
            
            if(rand1 < rand2) return 1;
            if(rand1 > rand2) return -1;
        }
        
        return 0; // Never happens but Netbeans thinks it might. So it won't compile without a return statement
   }

}
