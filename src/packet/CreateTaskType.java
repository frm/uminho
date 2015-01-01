package packet;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

public class CreateTaskType implements Serializable {
    //query
    public String q_name;
    public Map<String, Integer> q_itens;

    //reply
    public Collection<String> r_errors = new ArrayList<>();
}
