package async.echo;

import java.io.IOException;

/**
 * Created by frm on 14/10/15.
 */
public class Main {
    public static void main(String[] args) throws IOException {
        new Server(3000).listen();
    }
}
