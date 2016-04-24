
/**
 *
 * @author joaorodrigues
 */

import java.io.Serializable;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 *
 * @author joaorodrigues
 */
public class UserList implements Serializable{
    private HashSet<Integer> users;

    /**
     *
     */
    public UserList() {
        this.users = new HashSet<Integer>();
    }

    /**
     *
     * @param users
     */
    public UserList(HashSet<Integer> users) {
        this.users = new HashSet<Integer>();
        for(Integer i: users)
            (this.users).add(i);
    }

    /**
     *
     * @param userlist
     */
    public UserList(UserList userlist){
        this.users = new HashSet<Integer>();
        for( Integer i: userlist.getUsers() )
            (this.users).add(i);
    }

    /**
     *
     * @return
     */
    public HashSet<Integer> getUsers() {
        HashSet<Integer> copy = new HashSet<Integer>();
        for (int i : this.users)
            copy.add(i);

        return copy;
    }

    /**
     *
     * @param users
     */
    public void setUsers(HashSet<Integer> users) {
        this.users = users;
    }

    @Override
    public UserList clone() {
        UserList users2 = new UserList();
        try {
           users2 = new UserList(this);
        } catch (IllegalArgumentException e) {
            System.err.println("Unexisting users2 list");
            throw new IllegalArgumentException( e.getMessage() );
        }

        return users2;
    }

    @Override
    public String toString(){
        String str = "";
        for(Integer i : this.users )
            str += i.toString() + " ";

        return str;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        UserList f = (UserList) obj;
        return this.users.equals( f.getUsers() );
    }

    /**
     *
     * @param id
     */
    public void addUser(Integer id){
        users.add(id);
    }

    /**
     *
     * @param id
     * @return
     */
    public boolean removeUser(Integer id) {
        return users.remove(id);
    }

    /**
     *
     * @return
     */
    public int numberOfUsers(){
        return users.size();
    }

    /**
     *
     * @param id
     * @return
     */
    public boolean containsUser(int id){
        return users.contains(id);
    }

    /**
     *
     * @return
     */
    public Iterator<Integer> iterator()  {
        return this.users.iterator();
    }

    /**
     *
     * @return
     */
    public Set<Integer> toSet() {
        return this.getUsers();
    }
}