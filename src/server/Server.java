package server;

import warehouse.UnknownPacketException;
import warehouse.Warehouse;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;

public class Server {
    private final Warehouse warehouse = new Warehouse();
    private ServerSocket serverSocket;

    public Server(int startingPort) {
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
    }

    public Worker newWorker() throws IOException {
        return new Worker(serverSocket.accept(), warehouse);
    }

    private class Worker implements Runnable {
        private Socket socket;
        private Warehouse warehouse;

        ObjectOutputStream out;
        ObjectInputStream in;

        Worker(Socket s, Warehouse w) throws IOException {
            socket = s;
            warehouse = w;

            out = new ObjectOutputStream(socket.getOutputStream());
            in = new ObjectInputStream(socket.getInputStream());
        }

        private void send(Object obj) throws IOException {
            out.writeObject(obj);
            out.flush();
        }

        private Serializable receive() throws IOException, ClassNotFoundException {
            return (Serializable)in.readObject();
        }

        private void doCreateTask(CreateTask obj){

        }

        private void doFinishTask(FinishTask obj){

        }

        private void doListAll(ListAll obj){

        }

        private void doListWorking(ListWorking obj){

        }

        private void doLogin(Login obj){

        }

        private void doStore(Store obj){

        }

        private void doSubscribe(Subscribe obj){

        }

        @Override
        public void run() {
            try {
                Serializable obj = receive();

                if(obj instanceof CreateTask)
                    doCreateTask( (CreateTask)obj );
                else if(obj instanceof FinishTask)
                    doFinishTask( (FinishTask)obj );
                else if(obj instanceof ListAll)
                    doListAll( (ListAll)obj );
                else if(obj instanceof ListWorking)
                    doListWorking( (ListWorking)obj );
                else if(obj instanceof Login)
                    doLogin( (Login)obj );
                else if(obj instanceof Store)
                    doStore( (Store)obj );
                else if(obj instanceof Subscribe)
                    doSubscribe( (Subscribe)obj );
                else
                    throw new UnknownPacketException("Server received an unknown packet.");

            } catch (IOException e) {
                e.printStackTrace();
            } catch (ClassNotFoundException e) {
                e.printStackTrace();
            } catch (UnknownPacketException e) {
                e.printStackTrace();
            }
        }
    }

    public static void main(String[] args) throws IOException{
        Server server = new Server(4000);

        while(true)
            new Thread(server.newWorker()).start();
    }
}
