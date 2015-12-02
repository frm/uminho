package util;

import co.paralleluniverse.actors.ActorRef;

/**
 * Created by joaorodrigues on 1 Dec 15.
 */



public class Msg {
    public enum Type {OK, ERROR, CHAT, ADD, REMOVE, PM, GET_ROOM, ROOM, NEW_CHAT, GET_ROOM_USERS,ROOM_USERS, JOIN, LEAVE, GET_ROOMS, ROOMS, KICK, CLOSE}
    public final Type type;
    public final Object content;
    public final ActorRef sender;

    public Msg(Type type, Object content, ActorRef sender){
      this.type = type;
      this.content = content;
      this.sender = sender;
    }

}
