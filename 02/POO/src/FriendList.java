
import java.io.Serializable;
import java.util.Set;

/**
 * Class that holds information about the user friends, sent requests and received requests
 * @author frmendes
*/
public class FriendList implements Serializable{
        private UserList friends;                          // ids of actual frinds
        private UserList requestsSent;                     // ids of users whom requests were sent
        private UserList requestsReceived;                 // ids of users who sent requests

    /**
     * Empty constructor
     */
    public FriendList() {
            this.friends = new UserList();
            this.requestsSent = new UserList();
            this.requestsReceived = new UserList();
        }

    /**
     * Parameterized constructor
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

    /**
     * Returns the friend list
     * @return 
     */
        private UserList getFriends() {
            return this.friends.clone();
        }

        /**
         * Returns the sent requests list
         * @return 
         */
        private UserList getSentRequests() {
            return this.requestsSent.clone();
        }

        /**
         * Returns the received requests list
         * @return 
         */
        private UserList getReceivedRequests() {
            return this.requestsReceived.clone();
        }

    /**
     * Sets the friends List
     * @param f
     */
    public void setFriends(UserList f) {
            this.friends = f.clone();
        }

    /**
     * Sets the sent requests list
     * @param s 
     */
        private void setSentRequests(UserList s) {
            this.requestsSent = s.clone();
        }

        /**
         * Sets the received requests list
         * @param r 
         */
        private void setReceivedRequests(UserList r) {
            this.requestsReceived = r.clone();
        }

    /**
     * Returns the received requests list
     * @return
     */
    public Set<Integer> getRequests() {
            return this.requestsReceived.toSet();
        }

    /**
     * Returns the friend list
     * @return
     */
    public Set<Integer> getFriendList() {
            return this.friends.toSet();
        }

    /**
     * Sends a friend request
     * @param u
     */
    public void sendFriendRequest(User u) {
            sendFriendRequest( u.getId() );
        }

    /**
     * Sends a friend request
     * @param id
     */
    public void sendFriendRequest(int id) {
            this.requestsSent.addUser(id);
        }

    /**
     * Receives a friend request
     * @param u
     */
    public void receiveFriendRequest(User u) {
            receiveFriendRequest( u.getId() );
        }

    /**
     * Receives a friend request
     * @param id
     */
    public void receiveFriendRequest(int id) {
            this.requestsReceived.addUser(id);
        }

    /**
     * Accepts a friend request
     * @param u
     */
    public void acceptFriendRequest(User u) {
            acceptFriendRequest( u.getId() );
        }

    /**
     * Accepts a friend request
     * @param id
     */
    public void acceptFriendRequest(int id) {
            this.requestsReceived.removeUser(id);
            this.friends.addUser(id);
        }

    /**
     * Confirms a friend request
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
     * Determines if a user has received a particular request
     * @param u
     * @return
     */
    public boolean hasReceivedRequest(User u) {
            return hasReceivedRequest( u.getId() );
        }

    /**
     * Determines if a user has received a particular request
     * @param id
     * @return
     */
    public boolean hasReceivedRequest(int id) {
            return this.requestsReceived.containsUser(id);
        }

    /**
     * Determines if a user has any received friend requests
     * @return
     */
    public boolean hasFriendRequest() {
            return this.requestsReceived.numberOfUsers() != 0;
        }

        @Override
        public String toString() {
            StringBuilder str = new StringBuilder();
            str.append("Friends: ").append(this.friends);
            str.append("\nRequests Sent: ").append(this.requestsSent);
            str.append("\nRequests Received: ").append(this.requestsReceived);
            return str.toString();
        }

        @Override
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

        @Override
        public FriendList clone() {
            return new FriendList(this);
        }
}