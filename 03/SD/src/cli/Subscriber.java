package cli;

import asg.cliche.ShellFactory;
import packet.Subscribe;

public class Subscriber implements Runnable {
    Receiver<Subscribe> receiver;

    Subscriber(Receiver<Subscribe> receiver){
        this.receiver = receiver;
    }

    @Override
    public void run() {
        Subscribe s = receiver.get();
        // got a Subscribe packet

        StringBuilder sb = new StringBuilder();

        //check for errors
        if( s.r_errors.size() > 0 ){

            System.err.println("\nErrors in subscribe:");
            for(String e : s.r_errors)
                sb.append(e).append("\n");
            sb.append("\n");
            System.err.print(sb.toString());
            return;
        }

        // no errors, yey
        for(Integer i : s.q_ids){
            if(sb.length() != 0)
                sb.append(", ");
            sb.append(i);
        }
        sb.insert(0, "\nNotification: The requested tasks (");
        sb.append(") are finished.\n");

        System.out.print(sb.toString());
    }
}
