package future.ring;

import java.io.IOException;
import java.util.concurrent.ExecutionException;

/**
 * Created by frm on 16/10/15.
 */
public class Main {
    public static void main(String[] args) throws InterruptedException, ExecutionException, IOException {
        new future.ring.Server().listen();
    }
}
