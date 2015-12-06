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

    private HashMap<String ,Integer> ports;
    private HashMap<String, ZMQ.Socket> sockets;
    private Integer currPort;
    private final ZMQ.Context context = ZMQ.context(1);

    public NotificationHandler(){

        Random rand = new Random();

        ports = new HashMap<>();

        ports.put("UserList", rand.nextInt(8100)+8000);
        ports.put("UserState", rand.nextInt(8200)+8101);
        ports.put("RoomList", rand.nextInt(8300)+8201);
        ports.put("RoomState", rand.nextInt(8400)+8301);
        ports.put("RoomListRequest", rand.nextInt(8500)+8401);

        currPort = 8500;

        sockets = new HashMap<>();

        ZMQ.Socket socket;

        for (Map.Entry<String, Integer> entry : ports.entrySet()) {
            socket = context.socket(ZMQ.SUB);
            socket.bind("tcp://*:" + entry.getValue());
            sockets.put(entry.getKey(), socket);
        }
    }


    private void addRoomSocket(String name){
        Random rand = new Random();
        int port = rand.nextInt(currPort+99)+ (currPort+1);
        ports.put("r_"+name, port );
        currPort += 99;
        ZMQ.Socket socket = context.socket(ZMQ.SUB);
        socket.bind("tcp://*:" + port);
        sockets.put("r_"+name, socket);
    }

    private void removeRoomSocket(String name){
        sockets.get("r_"+name).unbind("tcp://*:" + ports.get("r_"+name));
        sockets.remove("r_"+name);
        ports.remove("r_"+name);
    }

    private String getPortListString(){
        ArrayList<String> strings = new ArrayList<>();
        for (Map.Entry<String, Integer> entry : ports.entrySet()) {
            strings.add(entry.getKey()+":"+entry.getValue());
        }

        return String.join(",", strings);
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
                        addRoomSocket(notification.field1);
                        sockets.get("RoomList").send(notification.toString());
                        return true;
                    case REMOVE:
                        removeRoomSocket(notification.field1);
                        sockets.get("RoomList").send(notification.toString());
                        return true;
                    case LOGIN:
                    case LOGOUT:
                        sockets.get("UserState").send(notification.toString());
                        return true;
                    case SIGNUP:
                        sockets.get("UserList").send(notification.toString());
                        return true;
                    case ROOM_LIST_REQUEST:
                        sockets.get("RoomListRequest").send(notification.toString());
                        notification.sender.send( new Msg(Msg.Type.PORT_LIST, getPortListString(), self()));
                        return true;
                }
                return false;
            }));
        return null;
    }
}
