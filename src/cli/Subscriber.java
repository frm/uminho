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
        StringBuilder sb = new StringBuilder();

        for(Integer i : s.q_ids){
            if(sb.length() != 0)
                sb.append(", ");
            sb.append(i);
        }

        sb.insert(0, "\nNotification: The requested tasks (");

        sb.append(") are finished.\n");
    }
}
