package server;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
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
    public Collection<String> r_errors = new ArrayList<>();
}

class StartTask implements Serializable{
    //query

    //reply
    public Collection<String> r_errors = new ArrayList<>();
}

class FinishTask implements Serializable {
    //query

    //reply
    public Collection<String> r_errors = new ArrayList<>();
}

class ListAll implements Serializable {
    //query

    //reply
    public Collection<String> r_errors = new ArrayList<>();
}

class ListWorking implements Serializable {
    //query

    //reply
    public Collection<String> r_errors = new ArrayList<>();
}

class Login implements Serializable {
    //query
    public Boolean createUser;
    public String username;
    public String password;

    //reply
    public Collection<String> r_errors = new ArrayList<>();
}

class Store implements Serializable {
    //query

    //reply
    public Collection<String> r_errors = new ArrayList<>();
}

class Subscribe implements Serializable {
    //query

    //reply
    public Collection<String> r_errors = new ArrayList<>();
}