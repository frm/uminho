package packet;

public class Login extends Packet {
    //query
    public Boolean q_createUser;
    public String q_username;
    public String q_password;

    // special
    public Integer id;

    public Login() {
        super();
        this.q_createUser = false;
        this.q_username = new String();
        this.q_password = new String();
        this.id = -1;
    }
}
