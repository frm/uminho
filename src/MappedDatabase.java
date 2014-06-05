
import java.util.Map;
import java.util.Set;

/*
 * Interface that defines what a mapped database should have
 * It defines the necessary interactions that allow a proper communication between a controller and the database
 */

/**
 *
 * @author frmendes
 */
public interface MappedDatabase<T> {
    /** Getter for the ID Map */
    public Map<Integer, T> getIdEntry();
    
    /** Returns a set with all the values of the database */
    public Set<T> all();
    
    /** Returns the element with the corresponding ID */
    public T findById(int id);
    
    /** Saves a user into a database */
    public void save(T t);
    
    /** Deletes the element with the corresponding ID */
    public void delete(int id);
    
    /** Deletes the corresponding element */
    public void delete(T t);
    
    
}
