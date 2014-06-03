
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


        public FriendList() {
            this.friends = new UserList();
            this.requestsSent = new UserList();
            this.requestsReceived = new UserList();
        }

        public FriendList(UserList f, UserList s, UserList r) {
            this.friends = f.clone();
            this.requestsSent = s.clone();
            this.requestsReceived = s.clone();
        }

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

        public void setFriends(UserList f) {
            this.friends = f.clone();
        }

        private void setSentRequests(UserList s) {
            this.requestsSent = s.clone();
        }

        private void setReceivedRequests(UserList r) {
            this.requestsReceived = r.clone();
        }

        public Set<Integer> getRequests() {
            return this.requestsReceived.toSet();
        }

        public Set<Integer> getFriendList() {
            return this.friends.toSet();
        }

        public void sendFriendRequest(User u) {
            sendFriendRequest( u.getId() );
        }

        public void sendFriendRequest(int id) {
            this.requestsSent.addUser(id);
        }

        public void receiveFriendRequest(User u) {
            receiveFriendRequest( u.getId() );
        }

        public void receiveFriendRequest(int id) {
            this.requestsReceived.addUser(id);
        }

        public void acceptFriendRequest(User u) {
            acceptFriendRequest( u.getId() );
        }

        public void acceptFriendRequest(int id) {
            this.requestsReceived.removeUser(id);
            this.friends.addUser(id);
        }

        public void confirmFriendRequest(User u) {
            confirmFriendRequest( u.getId() );
        }

        public void confirmFriendRequest(int id) {
            removeSentRequest(id);
            this.friends.addUser(id);
        }

        public void rejectFriendRequest(User u) {
            rejectFriendRequest( u.getId() );
        }

        public void rejectFriendRequest(int id) {
            this.requestsReceived.removeUser(id);
        }

        public void removeSentRequest(User u) {
            removeSentRequest( u.getId() );
        }

        public void removeSentRequest(int id) {
            this.requestsSent.removeUser(id);
        }

        public void deleteFriend(User u) {
            deleteFriend( u.getId() );
        }

        public void deleteFriend(int id) {
            this.friends.removeUser(id);
        }

        public boolean hasFriend(User u) {
            return hasFriend( u.getId() );
        }

        public boolean hasFriend(int id) {
            return this.friends.containsUser(id);
        }

        public boolean hasSentRequest(User u) {
            return hasSentRequest( u.getId() );
        }

        public boolean hasSentRequest(int id) {
            return this.requestsSent.containsUser(id);
        }

        public boolean hasReceivedRequest(User u) {
            return hasReceivedRequest( u.getId() );
        }

        public boolean hasReceivedRequest(int id) {
            return this.requestsReceived.containsUser(id);
        }

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