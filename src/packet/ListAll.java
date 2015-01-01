package packet;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

public class ListAll implements Serializable {
    //query
    //no query attributes, just the packet is enough

    //reply
    public Collection<String> r_errors = new ArrayList<>();
    public Map<String, Collection<Integer>> r_instances;
}
