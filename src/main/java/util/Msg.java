package util;

import co.paralleluniverse.actors.ActorRef;

/**
 * Created by joaorodrigues on 1 Dec 15.
 */



public class Msg {
    public enum Type {REF, TEXT, OK, ERROR, CHAT, CLOSE, ADD, REMOVE}
    public final Type type;
    public final Object content;
    public final ActorRef sender;

    public Msg(Type type, Object content, ActorRef sender){
      this.type = type;
      this.content = content;
      this.sender = sender;
    }

}
