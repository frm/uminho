package packet;

public class StartTask extends Packet {
    //query
    public String q_name;

    // special
    public Integer id;

    public StartTask() {
        super();
        this.q_name = new String();
        this.id = -1;
    }
}
