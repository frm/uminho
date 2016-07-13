package packet;


public class FinishTask extends Packet {
    //query
    public Integer q_taskID;

    public FinishTask() {
        super();
        this.q_taskID = -1;
    }
}
