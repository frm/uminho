
import java.util.HashSet;

/**
 *
 * @author frmendes
 */

public class User {
    private String name;
    private String password;
    private String email;
    private int id;
    private UserList friends;
    private UserInfo info;
    private Records records;
    
    public User() {
        this.name = "";
        this.password = "";
        this.email = "";
        this.id = -1;
        this.friends = new UserList(); 
        this.info = new UserInfo();
        this.records = new Records();
    }
    
    public User(String name, String password, String email, UserInfo info) {
        this.name = name;
        this.password = password;
        this.email = email;
        this.info = info.clone();
        this.friends = new UserList();
        this.id = -1;
        this.records = new Records();
    }

    public User(String name, String password, String email, UserList friendlist, UserInfo info, Records rec) {
        this.name = name;
        this.password = password;
        this.email = email;
        this.id = -1;
        this.friends = friendlist.clone();
        this.info = info.clone();
    }
    
    public User(User u) {        
        this.id = u.getId();
        this.name = u.getName();
        this.email = u.getEmail();
        this.password = u.getPassword();
        this.friends = u.getFriends();
        this.info = u.getInfo();
        this.records = u.getRecords();
    }
    
    /** For a given password, sees if it matches the users
    @param password given password
    @return true or false
    */
    public boolean matchPassword(String password) {
    return this.password.equals(password);
    }

    public String getEmail() {
        return email;
    }
    
/* Although it doesn't make sense to implement a getPassword method for security reasons
 * it is implemented as private for copy constructor (see public User (User u) )
 */
    private String getPassword() {
        return password;
    }

    public int getId() {
        return id;
    }

    public String getName() {
        return name;
    }
    
    public UserList getFriends() {
        return friends.clone();
    }
    
    public UserInfo getInfo(){
        return info.clone();
    }
    
    public Records getRecords(){
        return records.clone();
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public void setId(int id) {
        this.id = id;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setPassword(String password) {
        this.password = password;
    }
    
    public void setFriends(UserList friendlist){
        this.friends = friendlist.clone();
    }
    
    public void setInfo(UserInfo info){
        this.info = info.clone();
    }
    
    public void setRecords(Records rec){
        this.records = rec.clone();
    }

    public User clone() {
        return new User(this);
    }
    
    public String toString() {
        StringBuilder result = new StringBuilder();
        result.append("### Utilizador: ###");
        result.append("\nE-Mail: ");
        result.append(this.email);
        result.append("\nName: ");
        result.append(this.name);
        result.append("\nFriends: ");
        result.append(this.friends);
        result.append("\nInfo: ");
        result.append(this.info);
        result.append("\nRecords: ");
        result.append(this.records);

        return result.toString();
    }
    
    public boolean hasFriend(Integer id){
        return this.friends.hasFriend(id);
    }

    
    @Override
    public boolean equals(Object o) {
        if(this == o) return true;

        if(o == null || this.getClass() != o.getClass() ) return false;

        User u = (User) o;
        
       return (u.getEmail().equals(this.email) && u.getPassword().equals(this.password) && u.getName().equals(this.name) && u.getFriends().equals(this.friends) && u.getInfo().equals(this.info) && u.getRecords().equals(this.records));
    }

    
}
