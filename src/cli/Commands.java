package cli;

import asg.cliche.Command;
import packet.*;

import java.util.HashMap;
import java.util.HashSet;

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

        dispatcher.doLogin(lg);
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

        dispatcher.doCreateTaskType(ctt);
    }

    @Command(name = "startTask", abbrev = "st")
    public void startTask(String name){
        StartTask st = new StartTask();

        st.q_name = name;
        dispatcher.doStartTask(st);
    }


    @Command(name = "finishTask", abbrev = "ft")
    public void finishTask(int i){
        FinishTask ft = new FinishTask();

        ft.q_taskID = i;
        dispatcher.doFinishTask(ft);
    }

    @Command(name = "listAll", abbrev = "la")
    public void listAll(){
        ListAll la = new ListAll();

        dispatcher.doListAll(la);
    }

    @Command(name = "store", abbrev = "s")
    public void store(String name, int amount){
        Store s = new Store();

        s.q_name = name;
        s.q_quantity = amount;
        dispatcher.doStore(s);
    }

    @Command(name = "subscribe", abbrev = "sub")
    public void subscribe(int... argsI){
        Subscribe sb = new Subscribe();
        HashSet<Integer> ids = new HashSet<Integer>();

        for(int i : argsI)
            ids.add(i);

        sb.q_ids = new HashSet<>(ids);
        dispatcher.doSubscribe(sb);
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
