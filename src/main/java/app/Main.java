package app;

import server.Server;

import java.util.concurrent.ExecutionException;

/**
 * Created by frm on 28/11/15.
 */
public class Main {
    public static void main(String[] args) throws ExecutionException, InterruptedException {
        Server s = new Server();
        s.spawn();
        s.join();
    }
}
