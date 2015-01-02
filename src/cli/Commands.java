package cli;

import asg.cliche.Command;
import asg.cliche.ShellFactory;
import java.io.IOException;

public class Commands {
    private Dispatcher dispatcher;

    Commands(Dispatcher d){
        dispatcher = d;
    }

    @Command
    public int add(int a, int b) {
        return a + b;
    }
}
