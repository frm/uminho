package packet;

public class StartTask extends Packet {
    //query
    public String q_name;

    //reply
    public int r_taskId; 

    public StartTask() {
        super();
        this.q_name = new String();
        this.r_taskId = -1;
    }
}
