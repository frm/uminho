package packet;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

public class ListAll extends Packet {
    //query
    //no query attributes, just the packet is enough

    //reply
    public Map<String, Collection<Integer>> r_instances;

    // special
    public Integer id;

    public ListAll() {
        super();
        this.r_instances = new HashMap<>();
        this.id = -1;
    }
}
