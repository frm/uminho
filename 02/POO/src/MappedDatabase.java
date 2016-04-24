
import java.util.Map;
import java.util.Set;

/*
 * Interface that defines what a mapped database should have
 * It defines the necessary interactions that allow a proper communication between a controller and the database
 */

/**
 *
 * @author frmendes
 * @param <T>
 */
public interface MappedDatabase<T> {
    /** Getter for the ID Map
     * @return Map of ID, element
     */
    public Map<Integer, T> getIdEntry();
    
    /** Returns a set with all the values of the database
     * @return Set with all the values in the database
     */
    public Set<T> all();
    
    /** Returns the element with the corresponding ID
     * @param id id of the element
     * @return element with corresponding ID
     * @throws java.lang.Exception */
    public T findById(int id) throws Exception;
    
    /** Saves a user into a database
     * @param t Element to be saved
     */
    public void save(T t);
    
    /** Deletes the element with the corresponding ID
     * @param id id of the corresponding user
     * @throws java.lang.Exception
     */
    public void delete(int id) throws Exception;
    
    /** Deletes the corresponding element
     * @param t Element to be deleted
     * @throws java.lang.Exception
     */
    public void delete(T t) throws Exception;
    
    
}
