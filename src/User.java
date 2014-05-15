
import java.util.HashSet;

/**
 *
 * @author frmendes
 */

public class User extends BasicUser {
    private UserList friends;
    private UserInfo info;
    private Records records;
    
    public User() {
        super();
        this.friends = new UserList(); 
        this.info = new UserInfo();
        this.records = new Records();
    }
    
    public User(String name, String password, String email, UserInfo info) {
        super(name, password, email);
        this.info = info.clone();
        this.friends = new UserList();
        this.records = new Records();
    }

    public User(String name, String password, String email, UserList friendlist, UserInfo info, Records rec) {
        super(name, password, email);
        this.friends = friendlist.clone();
        this.info = info.clone();
        this.records = rec.clone();
    }
    
    public User(User u) {        
        super(u);
        this.friends = u.getFriends();
        this.info = u.getInfo();
        this.records = u.getRecords();
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
        result.append("### User: ###");
        result.append( super.toString() );
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
        
       return ( u.getFriends().equals(this.friends) && u.getInfo().equals(this.info) && u.getRecords().equals(this.records) && super.equals(o) );
    } 
}
