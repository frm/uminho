package warehouse;

/**
 * Created by joaorodrigues on 15 Dec 14.
 */
public class UnknownPacketException extends WarehouseException{
    public UnknownPacketException() { super("UnknownPacketException"); }

    public UnknownPacketException(String msg) { super("UnknownPacketException", msg); }

}
