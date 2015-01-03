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

    public Server(int startingPort, Pipe p) {
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

        Worker(Warehouse w, ObjectOutputStream o, ObjectInputStream i){
            warehouse = w;
            out = o;
            in = i;
        }

        protected void send(Object obj) throws IOException {
            out.writeObject(obj);
            out.flush();
        }

        protected Serializable receive() throws IOException, ClassNotFoundException {
            try {
                return (Serializable) in.readObject();
            }catch (IOException | ClassNotFoundException e){
                throw e;
            } catch (Exception e){
                // make end of stream or any other exceptions an IOexception to close the socket
                throw new IOException();
            }
        }

        protected void doCreateTaskType(CreateTaskType obj) throws IOException {
            try {
                warehouse.newTaskType(obj.q_name, obj.q_itens);
            } catch (ExistentTaskException e) {
                obj.r_errors.add(e.getUserMessage());
            } catch (InvalidItemQuantityException e) {
                obj.r_errors.add(e.getUserMessage());
            }

            send(obj);
        }

        protected void doStartTask(StartTask obj) throws IOException {
            try {
                warehouse.startTask(obj.q_name);
            } catch (WarehouseException e) {
                obj.r_errors.add(e.getUserMessage());
            }

            send(obj);
        }

        protected void doFinishTask(FinishTask obj) throws IOException {
            try {
                warehouse.endTask(obj.q_taskID);
            } catch (WarehouseException e) {
                obj.r_errors.add(e.getUserMessage());
            }

            send(obj);
        }

        protected void doListAll(ListAll obj) throws IOException {
            obj.r_instances = warehouse.getRunningTasks();

            send(obj);
        }

        protected boolean doLogin(Login obj) {
            boolean logged = false;
            String error = null;
            Server.userLock.lock();
            User u = Server.users.get(obj.q_username);

            if (obj.q_createUser) {
                if(u == null) {
                    u = new User(obj.q_username, obj.q_password);
                    Server.users.put(obj.q_username, u);
                    u.login();
                    logged = true;
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
                    u.login();
                    logged = true;
                }
            }

            obj.r_errors.add(error);
            Server.userLock.unlock();
            return logged;
        }

        protected void doStore(Store obj) throws IOException {
            try {
                warehouse.stockUp(obj.q_name, obj.q_quantity);
            } catch (InvalidItemQuantityException e) {
                obj.r_errors.add(e.getUserMessage());
            }

            send(obj);
        }

        protected void doSubscribe(Subscribe obj) {
            (new Thread(
                    new SubscriptionHandler(out, obj, warehouse) )
            ).start();
        }

        @Override
        public void run(){
            Boolean loggedin = false;

            // try to authenticate before anything else
            try {
                Serializable obj = receive();

                if (!(obj instanceof Login))
                    throw new UnknownPacketException("Server received an unexpected packet.");

                loggedin = doLogin((Login)obj);
                send(obj);
            } catch (ClassNotFoundException e) {
                e.printStackTrace();
            } catch (UnknownPacketException e) {
                e.printStackTrace();
            } catch (IOException e) {
                closeConnection();
            }

            while(loggedin && isStreamOK()) {
                try {
                    Serializable obj = receive();

                    if (obj instanceof CreateTaskType)
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
                    else
                        throw new UnknownPacketException("Server received an unexpected packet.");

                } catch (ClassNotFoundException e) {
                    e.printStackTrace();
                } catch (UnknownPacketException e) {
                    e.printStackTrace();
                } catch (IOException e) {
                    closeConnection();
                }
            }

            if(isStreamOK())
                closeConnection();
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private class LocalWorker extends Worker implements Runnable {

        LocalWorker(Warehouse w, OutputStream o, InputStream i) throws IOException {
            super(
                    w,
                    new ObjectOutputStream(o),
                    new ObjectInputStream(i)
            );
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
