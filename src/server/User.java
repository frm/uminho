package server;

/**
 * Created by mendes on 01/01/15.
 */

public class User {
    private String username;
    private String password;
    private int id;
    private boolean logged;
    private static int idCount = 1;

    public User(String uname, String pw) {
        this.username = uname;
        this.password = pw;
        this.logged = false;
        this.id = idCount++;
    }

    public boolean matchPassword(String password) {
        return this.password.equals(password);
    }

    public int hashCode() {
        return id;
    }

    public void login() {
        this.logged = true;
    }

    public void logout() {
        this.logged = false;
    }

    public boolean isLoggedIn() {
        return this.logged;
    }
}

