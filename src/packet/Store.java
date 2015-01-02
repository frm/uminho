package packet;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

public class Store implements Serializable {
    //query
    public String q_name;
    public Integer q_quantity;

    //reply
    public Collection<String> r_errors = new ArrayList<>();

    // special
    public Integer id;
}
