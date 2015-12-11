package notificationClient;

/**
 * Created by joaorodrigues on 10 Dec 15.
 */
import org.zeromq.ZMQ;
import util.MessageBuilder;

import java.io.FileNotFoundException;
import java.util.Scanner;

public class NotificationClient {
    public static void main(String[] args) {
        try {
            MessageBuilder.init();
        } catch (FileNotFoundException e) {
            System.out.println("\nYAML Message File Not Found");
        }

        int port = 5511;
        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket socket = context.socket(ZMQ.PUB);
        socket.bind("tcp://*:"+port);

        (new Thread(new NotificationReceiver(port))).start();


        Scanner scanner = new Scanner(System.in);
        while (true) {

            String s = scanner.nextLine();

            if (s == null) break;
            socket.send(s);
        }

        //socket.close();
        //context.term();
    }
}


