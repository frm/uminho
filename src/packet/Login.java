package packet;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

public class Login implements Serializable {
    //query
    public Boolean q_createUser;
    public String q_username;
    public String q_password;

    //reply
    public Collection<String> r_errors = new ArrayList<>();

    // special
    public Integer id;
}
