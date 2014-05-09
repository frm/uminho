/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author joaorodrigues
 */

import java.util.HashSet;

public class UserList {
    private HashSet<Integer> users;

    public UserList() {
        this.users = new HashSet<Integer>();
    }
    
    public UserList(HashSet<Integer> users) {
        for(Integer i: users)
            (this.users).add(i);
    }
    
    public UserList(UserList UserList){    
        for( Integer i: UserList.getusers() )
            (this.users).add(i);
    }

    public HashSet<Integer> getusers() {
        return (HashSet<Integer>)users.clone();
    }

    public void setusers(HashSet<Integer> users) {
        this.users = users;
    }
   
    @Override
    public UserList clone() {
        UserList users2 = new UserList();
        try {
            new UserList(this);
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
        return true;
    }
    
    public void addFriend(Integer id){
        users.add(id);
    }
    
    public boolean removeFriend(Integer id){
        return users.remove(id);
    }
    
    public Integer numberOfusers(){
        return users.size();
    }
   
    public boolean hasFriend(Integer id){
        return users.contains(id);
    }
    
    
}
