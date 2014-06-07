
import java.io.Serializable;
import java.util.Set;

/**
 *
 * @author frmendes
*/
public class FriendList implements Serializable{
        private UserList friends;                          // ids of actual frinds
        private UserList requestsSent;                     // ids of users whom requests were sent
        private UserList requestsReceived;                 // ids of users who sent requests

    /**
     *
     */
    public FriendList() {
            this.friends = new UserList();
            this.requestsSent = new UserList();
            this.requestsReceived = new UserList();
        }

    /**
     *
     * @param f
     * @param s
     * @param r
     */
    public FriendList(UserList f, UserList s, UserList r) {
            this.friends = f.clone();
            this.requestsSent = s.clone();
            this.requestsReceived = s.clone();
        }

    /**
     *
     * @param fl
     */
    public FriendList(FriendList fl) {
            this.friends = fl.getFriends();
            this.requestsSent = fl.getSentRequests();
            this.requestsReceived = fl.getReceivedRequests();
        }

        private UserList getFriends() {
            return this.friends.clone();
        }

        private UserList getSentRequests() {
            return this.requestsSent.clone();
        }

        private UserList getReceivedRequests() {
            return this.requestsReceived.clone();
        }

    /**
     *
     * @param f
     */
    public void setFriends(UserList f) {
            this.friends = f.clone();
        }

        private void setSentRequests(UserList s) {
            this.requestsSent = s.clone();
        }

        private void setReceivedRequests(UserList r) {
            this.requestsReceived = r.clone();
        }

    /**
     *
     * @return
     */
    public Set<Integer> getRequests() {
            return this.requestsReceived.toSet();
        }

    /**
     *
     * @return
     */
    public Set<Integer> getFriendList() {
            return this.friends.toSet();
        }

    /**
     *
     * @param u
     */
    public void sendFriendRequest(User u) {
            sendFriendRequest( u.getId() );
        }

    /**
     *
     * @param id
     */
    public void sendFriendRequest(int id) {
            this.requestsSent.addUser(id);
        }

    /**
     *
     * @param u
     */
    public void receiveFriendRequest(User u) {
            receiveFriendRequest( u.getId() );
        }

    /**
     *
     * @param id
     */
    public void receiveFriendRequest(int id) {
            this.requestsReceived.addUser(id);
        }

    /**
     *
     * @param u
     */
    public void acceptFriendRequest(User u) {
            acceptFriendRequest( u.getId() );
        }

    /**
     *
     * @param id
     */
    public void acceptFriendRequest(int id) {
            this.requestsReceived.removeUser(id);
            this.friends.addUser(id);
        }

    /**
     *
     * @param u
     */
    public void confirmFriendRequest(User u) {
            confirmFriendRequest( u.getId() );
        }

    /**
     *
     * @param id
     */
    public void confirmFriendRequest(int id) {
            removeSentRequest(id);
            this.friends.addUser(id);
        }

    /**
     *
     * @param u
     */
    public void rejectFriendRequest(User u) {
            rejectFriendRequest( u.getId() );
        }

    /**
     *
     * @param id
     */
    public void rejectFriendRequest(int id) {
            this.requestsReceived.removeUser(id);
        }

    /**
     *
     * @param u
     */
    public void removeSentRequest(User u) {
            removeSentRequest( u.getId() );
        }

    /**
     *
     * @param id
     */
    public void removeSentRequest(int id) {
            this.requestsSent.removeUser(id);
        }

    /**
     *
     * @param u
     */
    public void deleteFriend(User u) {
            deleteFriend( u.getId() );
        }

    /**
     *
     * @param id
     */
    public void deleteFriend(int id) {
            this.friends.removeUser(id);
        }

    /**
     *
     * @param u
     * @return
     */
    public boolean hasFriend(User u) {
            return hasFriend( u.getId() );
        }

    /**
     *
     * @param id
     * @return
     */
    public boolean hasFriend(int id) {
            return this.friends.containsUser(id);
        }

    /**
     *
     * @param u
     * @return
     */
    public boolean hasSentRequest(User u) {
            return hasSentRequest( u.getId() );
        }

    /**
     *
     * @param id
     * @return
     */
    public boolean hasSentRequest(int id) {
            return this.requestsSent.containsUser(id);
        }

    /**
     *
     * @param u
     * @return
     */
    public boolean hasReceivedRequest(User u) {
            return hasReceivedRequest( u.getId() );
        }

    /**
     *
     * @param id
     * @return
     */
    public boolean hasReceivedRequest(int id) {
            return this.requestsReceived.containsUser(id);
        }

    /**
     *
     * @return
     */
    public boolean hasFriendRequest() {
            return this.requestsReceived.numberOfUsers() != 0;
        }

        public String toString() {
            StringBuilder str = new StringBuilder();
            str.append("Friends: ").append(this.friends);
            str.append("\nRequests Sent: ").append(this.requestsSent);
            str.append("\nRequests Received: ").append(this.requestsReceived);
            return str.toString();
        }

        public boolean equals(Object o) {
            if (o == null) {
                return false;
            }

            if (getClass() != o.getClass()) {
                return false;
            }

            FriendList fl = (FriendList) o;

            return this.friends.equals( fl.getFriends() )
                && this.requestsSent.equals( fl.getSentRequests() )
                && this.requestsReceived.equals( fl.getReceivedRequests() );
        }

        public FriendList clone() {
            return new FriendList(this);
        }
}