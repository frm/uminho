package async.ring;

import java.io.IOException;

/**
 * Created by frm on 15/10/15.
 */
public class Main {
    public static void main(String[] args) throws IOException {
        new Server(3000).listen();
    }
}
