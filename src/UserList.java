/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author joaorodrigues
 */

import java.io.Serializable;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

public class UserList implements Serializable{
    private HashSet<Integer> users;

    public UserList() {
        this.users = new HashSet<Integer>();
    }

    public UserList(HashSet<Integer> users) {
        this.users = new HashSet<Integer>();
        for(Integer i: users)
            (this.users).add(i);
    }

    public UserList(UserList userlist){
        this.users = new HashSet<Integer>();
        for( Integer i: userlist.getUsers() )
            (this.users).add(i);
    }

    public HashSet<Integer> getUsers() {
        HashSet<Integer> copy = new HashSet<Integer>();
        for (int i : this.users)
            copy.add(i);
        
        return copy;
    }

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
        return true;
    }

    public void addUser(Integer id){
        users.add(id);
    }

    public boolean removeUser(Integer id){
        return users.remove(id);
    }

    public int numberOfUsers(){
        return users.size();
    }

    public boolean containsUser(Integer id){
        return users.contains(id);
    }

    public Iterator<Integer> iterator()  {
        return this.users.iterator();
    }

    public Set<Integer> toSet() {
        return this.getUsers();
    }
}