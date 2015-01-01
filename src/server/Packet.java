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
    public String q_name;

    //reply
    public Collection<String> r_errors = new ArrayList<>();
}

class FinishTask implements Serializable {
    //query
    public Integer q_taskID;

    //reply
    public Collection<String> r_errors = new ArrayList<>();
}

class ListAll implements Serializable {
    //query
    //no query attributes, just the packet is enough

    //reply
    public Collection<String> r_errors = new ArrayList<>();
    public Map<String, Collection<Integer>> r_instances;
}

class ListWorking implements Serializable {
    //query
    //no query attributes, just the packet is enough

    //reply
    public Collection<String> r_errors = new ArrayList<>();
    public Map<String, Collection<Integer>> r_instances;
}

class Login implements Serializable {
    //query
    public Boolean q_createUser;
    public String q_username;
    public String q_password;

    //reply
    public Collection<String> r_errors = new ArrayList<>();
}

class Store implements Serializable {
    //query
    public String q_name;
    public Integer q_quantity;

    //reply
    public Collection<String> r_errors = new ArrayList<>();
}

class Subscribe implements Serializable {
    //query
    public Collection<Integer> q_ids;

    //reply
    public Collection<String> r_errors = new ArrayList<>();
}