package packet;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

/**
 * Created by frmendes on 1/4/15.
 */
public class Packet implements Serializable {
    //reply
    public Collection<String> r_errors;
    public Collection<String> r_success;

    public Packet() {
        this.r_errors = new ArrayList<>();
        this.r_success = new ArrayList<>();
    }
}
