package server;

import java.io.Serializable;
import java.util.Map;

/*
all attributes starting with r_ are reply attributes, intended to be sent from server to client
all attributes starting with q_ are query attributes, intended to be sent from client to server
 */

class CreateTaskType implements Serializable{
    //query
    public String q_name;
    public Map<String, Integer> q_itens;

    //reply
    public String r_error;
}

class StartTask implements Serializable{

}

class FinishTask implements Serializable {

}

class ListAll implements Serializable {

}

class ListWorking implements Serializable {

}

class Login implements Serializable {

}

class Store implements Serializable {

}

class Subscribe implements Serializable {
    
}