package packet;

public class StartTask extends Packet {
    //query
    public String q_name;

    public StartTask() {
        super();
        this.q_name = new String();
    }
}
