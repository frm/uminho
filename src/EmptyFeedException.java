
/**Exception for when the friend feed is empty
 *
 * @author joaorodrigues
 */
public class EmptyFeedException extends Exception
{

    /**
     *
     */
    public EmptyFeedException(){
        super();
    }
    
    /**
     *
     * @param s
     */
    public EmptyFeedException(String s){
        super(s);
    }  
}

