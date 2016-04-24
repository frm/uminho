package notificationClient;

import org.zeromq.ZMQ;
import util.MessageBuilder;

/**
 * Created by joaorodrigues on 10 Dec 15.
 */
public class NotificationReceiver implements Runnable{
    private int port;

    public NotificationReceiver(int port){ this.port = port; }


    private void subscribeAll(ZMQ.Socket notifications){
        notifications.subscribe("ROOM_LIST:".getBytes());
        notifications.subscribe("ROOM_USER_LIST:".getBytes());
        notifications.subscribe("GLOBAL_ROOM_MESSAGES:".getBytes());
        notifications.subscribe("USER_LIST:".getBytes());
        notifications.subscribe("USER_STATE:".getBytes());
    }

    private void unsubscribeAll(ZMQ.Socket notifications){
        notifications.unsubscribe("ROOM_LIST:".getBytes());
        notifications.unsubscribe("ROOM_USER_LIST:".getBytes());
        notifications.unsubscribe("GLOBAL_ROOM_MESSAGES:".getBytes());
        notifications.unsubscribe("USER_LIST:".getBytes());
        notifications.unsubscribe("USER_STATE:".getBytes());
        System.out.println("\n"+MessageBuilder.message(MessageBuilder.UNSUB_ALL));
    }

    @Override
    public void run() {
        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket commands = context.socket(ZMQ.SUB);
        commands.connect("tcp://localhost:" + port);
        ZMQ.Socket notifications = context.socket(ZMQ.SUB);
        notifications.connect("tcp://localhost:3001");

        commands.subscribe("/sub".getBytes());
        commands.subscribe("/unsub".getBytes());

        ZMQ.Poller poller = new ZMQ.Poller(2);
        poller.register(commands, ZMQ.Poller.POLLIN);
        poller.register(notifications, ZMQ.Poller.POLLIN);

        System.out.print("\nYou can subscribe to:\n" +
                "ROOM_LIST__________________Created and Removed Rooms\n" +
                "ROOM_USER_LIST_____________User joins and leaves\n" +
                "ROOM_MESSAGES_<roomName>___Messages in a room\n" +
                "GLOBAL_ROOM_MESSAGES_______Messages in all rooms\n"+
                "USER_LIST__________________Signups\n" +
                "USER_STATE_________________Login and Logout\n" +
                "ALL________________________Every event");


        while (true) {
            byte[] text;
            poller.poll();
            if (poller.pollin(0)) {
                text = commands.recv();
                String[] params = new String(text).split(" ");
                if(params.length != 2){
                    System.out.println("\n"+MessageBuilder.message(MessageBuilder.INVALID_PARAMS));
                    continue;
                }

                switch (params[0]) {
                    case "/sub":
                        if(params[1].equals("ALL"))
                            subscribeAll(notifications);
                        else
                            notifications.subscribe((params[1]+":").getBytes());
                        break;
                    case "/unsub":
                        if(params[1].equals("ALL"))
                            unsubscribeAll(notifications);
                        else
                        notifications.unsubscribe((params[1]+":").getBytes());
                        break;
                    default:
                        System.out.println("\n"+MessageBuilder.message(MessageBuilder.INVALID_COMMAND));
                        break;
                }

            }
            if (poller.pollin(1)) {
                text = notifications.recv();
                System.out.println("\n"+(new String(text).split(":")[1]));
            }
        }
        //socket.close();
        //context.term();
    }
}
