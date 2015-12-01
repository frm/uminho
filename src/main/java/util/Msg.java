package util;

import co.paralleluniverse.actors.ActorRef;

/**
 * Created by joaorodrigues on 1 Dec 15.
 */



public class Msg {
    public enum Type {REF, TEXT, OK, ERROR, JOIN, CHAT, LEAVE, CLOSE, CREATE, REMOVE}
    private final Type type;
    private final Object content;
    private final ActorRef sender;

    public Msg(Type type, Object content, ActorRef sender){
      this.type = type;
      this.content = content;
      this.sender = sender;
    }

    public Type getType() {
        return type;
    }

    public Object getContent() {
        return content;
    }

    public ActorRef getSender() {
        return sender;
    }
}
