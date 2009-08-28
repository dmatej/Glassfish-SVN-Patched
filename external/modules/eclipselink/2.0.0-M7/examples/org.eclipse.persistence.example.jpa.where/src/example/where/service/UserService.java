package example.where.service;

import example.where.model.Group;
import example.where.model.User;

public interface UserService {

    public boolean isUserIdInUse(String userId);

    public User createNewUser(String id, String firstName, String lastName, Group[] initialGroups);

    public void removeUser(User user);

    public User addGroupToUser(User user, Group group);

    public User removeGroupFromUser(User user, Group group);

    public Group createGroup(String name);

    public void removeGroup(Group group);

}
