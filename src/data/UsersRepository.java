/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package data;

import java.util.HashMap;
import models.User;

/**
 *
 * @author frmendes
 */
public class UsersRepository extends AbstractRepository<User> {
    
    private HashMap<Integer, User> users;
    private String username;
    private String password;
    private String url;
    
    private static final String DB_TABLE = "Users";
    
    private static final HashMap<String, String> COLUMN_ATTR = new HashMap<String, String>() {{
       put("username", "username");
       put("password", "password");
       put("id", "code");
       put("name", "name");
    }};
    
    public UsersRepository(String username, String password, String url) {
        this.users = new HashMap<>();
        this.username = username;
        this.password = password;
        this.url = url;
    }
    
    @Override
    public User find(int id) {
        return users.get(id);
    }
    
    @Override
    public void save(User u) {
        users.put(u.getId(), u);
    }
    
    @Override
    public String getColumnFor(String attribute) {
        return COLUMN_ATTR.get(attribute);                
    }
    
    @Override
    public String getTableName() {
        return DB_TABLE;
    }
}
