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
        for(Integer i: friends){
            (this.friends).add(i);
        }
    }
    
    public FriendList(FriendList friendlist){    
        for(Integer i: friendlist.getFriends() ){
            (this.friends).add(i);
        }
    }

    public HashSet<Integer> getFriends() {
        return friends;
    }

    public void setFriends(HashSet<Integer> friends) {
        this.friends = friends;
    }
   
    @Override
    public FriendList clone(){
        return new FriendList(this);
    }
    
    @Override
    public String toString(){
        return friends.toString();
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
    
    
}
