package future.echo;

import java.io.IOException;
import java.util.concurrent.ExecutionException;

/**
 * Created by frm on 15/10/15.
 */
public class Main {
    public static void main(String[] args) throws IOException, ExecutionException, InterruptedException {
        new Server().listen();
    }
}
