package cli;

import asg.cliche.ShellFactory;
import server.Server;

import java.io.IOException;

public class Main {
    private static Boolean isServer = false;
    private static Integer port;

    private static void printUsage(){
        System.out.print("Invalid arguments.\n"
                        + "Arguments: [-s] port\n"
                        + "s    Run the application in server mode\n"
                        + "port  Server listening port\n"
        );
    }

    private static Boolean validArguments(String[] args){
        Boolean resultado = false;

        if(args.length == 1){
            isServer = false;
            resultado = true;
            try {
                port = Integer.parseInt(args[0]);
            } catch (NumberFormatException e) {
                resultado = false;
            }
        }else if(args.length == 2 && args[0].equals("-s")){
            isServer = true;
            resultado = true;
            try {
                port = Integer.parseInt(args[1]);
            } catch (NumberFormatException e) {
                resultado = false;
            }
        }

        return resultado;
    }

    public static void main(String[] args) {
        if(!validArguments(args)){
            printUsage();
            return;
        }

        try {
            Dispatcher dispatcher = isServer ? new Dispatcher(Server.startNewServer(port)) : new Dispatcher(port);

            try {
                ShellFactory.createConsoleShell("", "", new Commands(dispatcher))
                        .commandLoop();
            } catch (IOException e) {
                // ignore io exceptions
            } catch (Exception e) {
                // debug the others
                e.printStackTrace();
            }

            dispatcher.terminate();
        } catch (IOException e) {
            e.printStackTrace();
        }

        System.err.println("Terminated.");
    }
}
