package packet;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

public class FinishTask implements Serializable {
    //query
    public Integer q_taskID;

    //reply
    public Collection<String> r_errors = new ArrayList<>();
}
