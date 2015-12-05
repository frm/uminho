package notification;

import co.paralleluniverse.actors.ActorRef;
import co.paralleluniverse.actors.BasicActor;
import co.paralleluniverse.fibers.SuspendExecution;
import communication.Msg;
import org.zeromq.ZMQ;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;

/**
 * Created by joaorodrigues on 5 Dec 15.
 */
public class NotificationHandler extends BasicActor<Notification, Void> {

    private HashMap<String ,Integer> ports;
    private HashMap<String, ZMQ.Socket> sockets;

    public NotificationHandler(){
        ZMQ.Context context = ZMQ.context(1);
        Random rand = new Random();

        ports = new HashMap<>();

        ports.put("UserList", rand.nextInt(8100)+8000);
        ports.put("UserState", rand.nextInt(8200)+8101);
        ports.put("RoomList", rand.nextInt(8300)+8201);
        ports.put("RoomState", rand.nextInt(8400)+8301);

        sockets = new HashMap<>();

        ZMQ.Socket socket;

        for (Map.Entry<String, Integer> entry : ports.entrySet()) {
            socket = context.socket(ZMQ.SUB);
            socket.bind("tcp://*:" + entry.getValue());
            sockets.put(entry.getKey(), socket);
        }
    }


    @Override
    protected Void doRun() throws InterruptedException, SuspendExecution {

        while(
            receive(notification -> {
                switch (notification.type) {
                    case JOIN:
                    case LEAVE:
                        sockets.get("RoomState").send(notification.toString());
                        return true;
                    case CREATE:
                    case REMOVE:
                        sockets.get("RoomList").send(notification.toString());
                        return true;
                    case LOGIN:
                    case LOGOUT:
                        sockets.get("UserState").send(notification.toString());
                        return true;
                    case SIGNUP:
                        sockets.get("UserList").send(notification.toString());
                        return true;
                }
                return false;
            }));
        return null;
    }
}
