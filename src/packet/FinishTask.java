package packet;


public class FinishTask extends Packet {
    //query
    public Integer q_taskID;

    // special
    public Integer id;

    public FinishTask() {
        super();
        this.q_taskID = -1;
        this.id = -1;
    }
}
