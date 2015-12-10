package notification;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import communication.Msg;
import org.zeromq.ZMQ;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

/**
 * Created by joaorodrigues on 5 Dec 15.
 */
public class NotificationHandler extends BasicActor<Notification, Void> {

    private final ZMQ.Context context = ZMQ.context(1);
    private int port;
    private ZMQ.Socket socket;

    public NotificationHandler(){
        port = 3001;
        socket = context.socket(ZMQ.PUB);
        socket.bind("tcp://*:"+port);

    }

    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {

        while(
            receive(notification -> {
                switch (notification.type) {
                    case JOIN:
                    case LEAVE:
                        socket.send("ROOM_USER_LIST:"+notification.toString());
                        return true;
                    case CREATE:
                    case REMOVE:
                        socket.send("ROOM_LIST:"+notification.toString());
                        return true;
                    case LOGIN:
                    case LOGOUT:
                        socket.send("USER_STATE:"+notification.toString());
                        return true;
                    case SIGNUP:
                        socket.send("USER_LIST:"+notification.toString());
                        return true;
                    case ROOM_LIST_REQUEST:
                        socket.send("ROOM_MESSAGES_"+notification.field2+":"+notification.toString());
                }
                System.out.println("RECEIVED A NOTIFICATION");
                return false;
            }));
        return null;
    }
}
