package packet;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

public class StartTask implements Serializable {
    //query
    public String q_name;

    //reply
    public Collection<String> r_errors = new ArrayList<>();
}
