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

public class FriendList {
    private HashSet<Integer> friends;

    public FriendList() {
        this.friends = new HashSet<Integer>();
    }
    
    public FriendList(HashSet<Integer> friends) {
        for(Integer i: friends)
            (this.friends).add(i);
    }
    
    public FriendList(FriendList friendlist){    
        for( Integer i: friendlist.getFriends() )
            (this.friends).add(i);
    }

    public HashSet<Integer> getFriends() {
        return friends;
    }

    public void setFriends(HashSet<Integer> friends) {
        this.friends = friends;
    }
   
    @Override
    public FriendList clone() {
        FriendList friend = new FriendList();
        try {
            new FriendList(this);
        } catch (IllegalArgumentException e) {
            System.err.println("Unexisting friend list");
            throw new IllegalArgumentException( e.getMessage() );
        }
        
        return friend;
    }
    
    @Override
    public String toString(){
        String str = "";
        for(Integer i : this.friends )
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
        FriendList f = (FriendList) obj;
        return true;
    }
    
    public void addFriend(Integer id){
        friends.add(id);
    }
    
    public boolean removeFriend(Integer id){
        return friends.remove(id);
    }
    
    public Integer numberOfFriends(){
        return friends.size();
    }
   
    public boolean hasFriend(Integer id){
        return friends.contains(id);
    }
    
    
}
