package packet;

public class Store extends Packet {
    //query
    public String q_name;
    public Integer q_quantity;

    public Store() {
        super();
        this.q_name = new String();
        this.q_quantity = 0;
    }
}
