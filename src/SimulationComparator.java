
import java.io.Serializable;
import java.util.Comparator;

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
            int rand1;
            int rand2;
            
            do {
                rand1 = (int)(Math.random() * 100);
                rand2 = (int)(Math.random() * 100);
            } while(rand1 == rand2);
            
            if(rand1 < rand2) return 1;
            if(rand1 > rand2) return -1;
        }
        
        return 0; // Never happens but Netbeans thinks it might. So it won't compile without a return statement
   }

}
