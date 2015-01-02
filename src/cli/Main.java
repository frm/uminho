package cli;

import asg.cliche.Command;
import asg.cliche.ShellFactory;
import java.io.IOException;

public class Main {
    public static void main(String[] args) throws IOException {
        // args: [-s] port
        // -s    Iniciar em modo servidor
        // port  Porta para o socket


        Dispatcher dispatcher = new Dispatcher(1);

        ShellFactory.createConsoleShell("cliche", "", new Commands(dispatcher))
                .commandLoop();
    }
}
