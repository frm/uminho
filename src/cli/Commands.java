package cli;

import asg.cliche.Command;
import packet.*;

import java.util.HashMap;
import java.util.HashSet;
import java.util.*;

public class Commands {
    private Dispatcher dispatcher;

    Commands(Dispatcher d){
        dispatcher = d;
    }

    @Command(name = "login", abbrev = "lg")
    public void login(boolean cUser, String username, String password){
        Login lg = new Login();

        lg.q_createUser = cUser;
        lg.q_username = username;
        lg.q_password = password;

        lg = dispatcher.doLogin(lg);

        if(lg.r_errors != null) {
            System.err.println("\nUps");
            for (String s : lg.r_errors)
                System.err.println(s);
            return;
        }
    }

    @Command(name="createTaskType", abbrev = "ctt")
    public void createTaskType(String name, String... argsS){
        HashMap<String, Integer> itens = new HashMap<>();
        boolean shouldStr = true;
        String str="";
        int i;

        for(String s : argsS) {
            if (shouldStr) {
                shouldStr = false;
                str = s;
            }
            else if(!shouldStr && ((i = goodInput(s)) > 0) ){
                shouldStr = true;
                itens.put(str,i);
            }
            else
                return;
        }

        if(itens.isEmpty()){
            System.out.println("Bad input. Please enter need quantity");
            return;
        }

        CreateTaskType ctt = new CreateTaskType();
        ctt.q_name = name;
        ctt.q_itens = itens;

        ctt = dispatcher.doCreateTaskType(ctt);

        if(ctt.r_errors != null) {
            System.err.println("\nUps");
            for (String s : ctt.r_errors)
                System.err.println(s);
            return;
        }
    }

    @Command(name = "startTask", abbrev = "st")
    public void startTask(String name){
        StartTask st = new StartTask();

        st.q_name = name;
        st = dispatcher.doStartTask(st);

        if(st.r_errors != null) {
            System.err.println("\nUps");
            for (String s : st.r_errors)
                System.err.println(s);
            return;
        }
    }


    @Command(name = "finishTask", abbrev = "ft")
    public void finishTask(int i){
        FinishTask ft = new FinishTask();

        ft.q_taskID = i;
        ft = dispatcher.doFinishTask(ft);

        if(ft.r_errors != null) {
            System.err.println("\nUps");
            for (String s : ft.r_errors)
                System.err.println(s);
            return;
        }
    }

    @Command(name = "listAll", abbrev = "la")
    public void listAll(){
        ListAll la = new ListAll();

        la = dispatcher.doListAll(la);

        if(la.r_errors != null) {
            System.err.println("\nUps");
            for (String s : la.r_errors)
                System.err.println(s);
            return;
        }

        for(String t : la.r_instances.keySet()) {
            System.out.println("Tipo: " + t);
            Collection c = la.r_instances.get(t);
            for(Object i : c)
                System.out.println(" >id: " + i);
            }
    }

    @Command(name = "store", abbrev = "s")
    public void store(String name, int amount){
        Store s = new Store();

        s.q_name = name;
        s.q_quantity = amount;
        s = dispatcher.doStore(s);

        if(s.r_errors != null) {
            System.err.println("\nUps");
            for (String str : s.r_errors)
                System.err.println(str);
            return;
        }
    }

    @Command(name = "subscribe", abbrev = "sub")
    public void subscribe(int... argsI){
        Subscribe sb = new Subscribe();
        HashSet<Integer> ids = new HashSet<Integer>();

        for(int i : argsI)
            ids.add(i);

        sb.q_ids = new HashSet<>(ids);
        sb = dispatcher.doSubscribe(sb);
    }

    public int goodInput(String str){
        int res;

        try{
            res = Integer.parseInt(str);
        } catch(NumberFormatException e){
            System.out.println("\nBad Input. Need quantity must be a number");
            return -1;
        }
            return res;
    }
}
