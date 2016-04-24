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

    /**
     * Constructor
     * @param uname username to be given
     * @param pw corresponding password
     */
    public User(String uname, String pw) {
        this.username = uname;
        this.password = pw;
        this.logged = false;
        this.id = idCount++;
    }

    /**
     * Checks if the user password matches with the given password
     * @param password password to be matched
     * @return
     */
    public boolean matchPassword(String password) {
        return this.password.equals(password);
    }

    @Override
    public int hashCode() {
        return id;
    }

    /**
     * Set user as logged in
     */
    public void login() {
        this.logged = true;
    }

    /**
     * Set user as logged out
     */
    public void logout() {
        this.logged = false;
    }

    /**
     * Checks if the user is logged in
     * @return
     */
    public boolean isLoggedIn() {
        return this.logged;
    }

    /**
     *
     * @return user id
     */
    public int getId() {
        return this.id;
    }

    /**
     *
     * @return username
     */
    public String getUsername() {
        return this.username;
    }
}

