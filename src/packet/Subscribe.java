package packet;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

public class Subscribe implements Serializable {
    //query
    public Collection<Integer> q_ids;

    //reply
    public Collection<String> r_errors = new ArrayList<>();
}