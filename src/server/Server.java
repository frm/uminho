package server;

import cli.Pipe;
import packet.*;
import warehouse.*;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.*;
import java.util.concurrent.locks.ReentrantLock;

public class Server {
    private final Warehouse warehouse = new Warehouse();
    private ServerSocket serverSocket;
    private static Map<String, User> users;
    private static ReentrantLock userLock;
    private final Pipe pipe;

    public Server(int startingPort, Pipe p) throws IOException {
        while (true) {
            try {
                this.serverSocket = new ServerSocket(startingPort);
                System.err.println("Server socket created on port " + startingPort);
                break;
            } catch (IOException e) {
                System.err.println("Port " + startingPort + " occupied. Trying again...");
                startingPort++;
            }
        }

        pipe = p;
        users = new HashMap<String, User>();
        userLock = new ReentrantLock();


        final Server self = this;
        new Thread(
            new Runnable() {
                public void run() {
                    while(!serverSocket.isClosed()) {
                        try {
                            new Thread(self.newRemoteWorker()).start();
                        } catch (IOException e) {
                            //e.printStackTrace();
                        }
                    }
                }
            }
        ).start();

        LocalWorker localWorker = new LocalWorker(warehouse, p.getOut(), p.getIn());
        new Thread(localWorker).start();
    }

    public void stop(){
        try {
            serverSocket.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private abstract class Worker implements Runnable {
        protected Warehouse warehouse;

        protected ObjectOutputStream out;
        protected ObjectInputStream in;

        protected abstract void closeConnection();
        protected abstract Boolean isStreamOK();

        protected User currentUser;

        protected final Sender sender;

        Worker(Warehouse w, ObjectOutputStream o, ObjectInputStream i){
            warehouse = w;
            out = o;
            in = i;
            sender = new Sender(out);
            new Thread(sender).start();
        }

        protected void send(Packet p) throws IOException {
            sender.send(p);
        }

        protected Packet receive() throws IOException, ClassNotFoundException {
            try {
                return (Packet) in.readObject();
            }catch (IOException | ClassNotFoundException e){
                throw e;
            } catch (Exception e){
                // make end of stream or any other exceptions an IOException to close the socket
                throw new IOException();
            }
        }

        protected void doCreateTaskType(CreateTaskType obj) throws IOException {
            try {
                warehouse.newTaskType(obj.q_name, obj.q_itens);
                obj.r_success.add("Created a new Task Type");
            } catch (ExistentTaskException e) {
                obj.r_errors.add(e.getUserMessage());
            } catch (InvalidItemQuantityException e) {
                obj.r_errors.add(e.getUserMessage());
            }

            send(obj);
        }

        protected void doStartTask(StartTask obj) throws IOException {
            try {
                warehouse.startTask(obj.q_name, currentUser.getId());
                obj.r_success.add("Started new task!");
            } catch (WarehouseException e) {
                obj.r_errors.add(e.getUserMessage());
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            send(obj);
        }

        protected void doFinishTask(FinishTask obj) throws IOException {
            try {
                warehouse.endTask(obj.q_taskID, currentUser.getId());
                obj.r_success.add("Task is finished!");
            } catch (WarehouseException e) {
                obj.r_errors.add(e.getMessage());
            }

            send(obj);
        }

        protected void doListAll(ListAll obj) throws IOException {
            obj.r_instances = warehouse.getRunningTasks();

            send(obj);
        }

        protected boolean doLogin(Login obj) throws IOException {
            boolean logged = false;
            String error = null;
            Server.userLock.lock();

            User u = Server.users.get(obj.q_username);

            if (obj.q_createUser) {
                if(u == null) {
                    if (currentUser != null)
                        currentUser.logout();

                    u = new User(obj.q_username, obj.q_password);
                    Server.users.put(obj.q_username, u);
                    u.login();
                    logged = true;
                    currentUser = u;
                    obj.r_success.add("Logged in!");
                }
                else
                    error = "Username already exists";
            }
            else {
                if(u == null)
                    error = "User does not exist";
                else if(u.isLoggedIn())
                    error = "Already logged in";
                else if(!u.matchPassword(obj.q_password))
                    error = "Invalid username/password";
                else {
                    if (currentUser != null)
                        currentUser.logout();

                    u.login();
                    logged = true;
                    currentUser = u;
                    obj.r_success.add("Logged in!");
                }
            }

            if(error != null)
                obj.r_errors.add(error);
            Server.userLock.unlock();
            send(obj);
            return logged;
        }

        protected void doStore(Store obj) throws IOException {
            try {
                warehouse.stockUp(obj.q_name, obj.q_quantity);
                obj.r_success.add("Added to stock!");
            } catch (InvalidItemQuantityException e) {
                obj.r_errors.add(e.getUserMessage());
            }

            send(obj);
        }

        protected void doSubscribe(Subscribe obj) {
            (new Thread(
                    new SubscriptionHandler(sender, obj, warehouse) )
            ).start();
        }

        @Override
        public void run(){
            Boolean loggedin = false;

            // try to authenticate before anything else
            while (!loggedin && isStreamOK()) {
                try {
                    Packet obj = receive();

                    if (!(obj instanceof Login)) {
                        obj.r_errors.add("You need to login first.");
                        send(obj);
                        continue;
                    }

                    loggedin = doLogin((Login) obj);
                } catch (ClassNotFoundException e) {
                    e.printStackTrace();
                } catch (IOException e) {
                    if (currentUser != null) {
                        Server.userLock.lock();
                        users.get(currentUser.getUsername()).logout();
                        Server.userLock.unlock();
                        closeConnection();
                    }
                }
            }

            while(loggedin && isStreamOK()) {
                try {
                    Packet obj = receive();

                    if (obj instanceof Login)
                        doLogin((Login) obj);
                    else if (obj instanceof CreateTaskType)
                        doCreateTaskType((CreateTaskType) obj);
                    else if (obj instanceof StartTask)
                        doStartTask((StartTask) obj);
                    else if (obj instanceof FinishTask)
                        doFinishTask((FinishTask) obj);
                    else if (obj instanceof ListAll)
                        doListAll((ListAll) obj);
                    else if (obj instanceof Store)
                        doStore((Store) obj);
                    else if (obj instanceof Subscribe)
                        doSubscribe((Subscribe) obj);
                    else {
                        throw new UnknownPacketException("Server received an unexpected packet.");
                    }

                } catch (ClassNotFoundException e) {
                    e.printStackTrace();
                } catch (UnknownPacketException e) {
                    e.printStackTrace();
                } catch (IOException e) {
                    Server.userLock.lock();
                    users.get(currentUser.getUsername()).logout();
                    Server.userLock.unlock();
                    closeConnection();
                }
            }

            if(isStreamOK()) {
                Server.userLock.lock();
                users.get(currentUser.getUsername()).logout();
                Server.userLock.unlock();
                closeConnection();
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private class LocalWorker extends Worker implements Runnable {

        LocalWorker(Warehouse w, ObjectOutputStream o, ObjectInputStream i) throws IOException {
            super(w, o, i);
        }

        @Override
        protected Boolean isStreamOK() {
            return out != null && in != null;
        }

        @Override
        protected void closeConnection() {
            try {
                out.close();
            } catch (IOException e) {}
            out = null;

            try {
                in.close();
            } catch (IOException e) {}
            in = null;
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private RemoteWorker newRemoteWorker() throws IOException {
        return new RemoteWorker(serverSocket.accept(), warehouse);
    }

    private class RemoteWorker extends Worker implements Runnable {
        private Socket socket;

        RemoteWorker(Socket s, Warehouse w) throws IOException {
            super(
                    w,
                    new ObjectOutputStream(s.getOutputStream()),
                    new ObjectInputStream(s.getInputStream())
            );

            socket = s;
        }

        @Override
        protected void closeConnection(){
            try {
                socket.shutdownOutput(); // Sends the 'FIN' on the network
            } catch (Exception e) {} // for when the stream is somehow damaged

            try {
                InputStream is = socket.getInputStream(); // obtain stream
                while (is.read() >= 0) ; // "read()" returns '-1' when the 'FIN' is reached
            } catch (Exception e) {} // for when the stream is somehow damaged

            try {
                socket.close(); // Now we can close the Socket
            } catch (Exception e) {} // for when something is somehow damaged

            socket = null; //now it's closed!
        }

        @Override
        protected Boolean isStreamOK() {
            return socket != null && !socket.isClosed();
        }
    }
}
