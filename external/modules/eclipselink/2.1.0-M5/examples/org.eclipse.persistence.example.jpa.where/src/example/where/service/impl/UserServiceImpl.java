package example.where.service.impl;

import javax.persistence.EntityManager;

import example.where.model.Group;
import example.where.model.User;
import example.where.service.UserService;

public class UserServiceImpl extends PersistenceService implements UserService {
    
    public UserServiceImpl(ServiceFactory factory) {
        super(factory);
    }

    public boolean isUserIdInUse(String userId) {
        EntityManager em = getEMF().createEntityManager();

        try {
            return em.find(User.class, userId) != null;
        } finally {
            em.close();
        }
    }

    /**
     * 
     * @param em
     * @param id
     * @param firstName
     * @param lastName
     * @param initialGroups
     * @return
     */
    public User createNewUser(String id, String firstName, String lastName, Group[] initialGroups) {
        EntityManager em = getEMF().createEntityManager();

        try {
            User user = new User(id, firstName, lastName);

            em.persist(user);

            for (int index = 0; initialGroups != null && index < initialGroups.length; index++) {
                Group groupWC = em.find(Group.class, initialGroups[index]);

                if (groupWC == null) {
                    throw new IllegalStateException("User.createNewUser:: Group not found in this context: " + initialGroups[index]);
                }

                user.addGroup(groupWC);
            }

            return user;
        } finally {
            em.close();
        }
    }

    /**
     * 
     * @param em
     * @param user
     */
    public void removeUser(User user) {
        EntityManager em = getEMF().createEntityManager();

        try {
            User userWC = em.find(User.class, user.getId());

            if (userWC == null) {
                throw new IllegalStateException("User.removeUser:: User does not exist in this context: " + user);
            }

            em.remove(userWC);
        } finally {
            em.close();
        }
    }

    /**
     * 
     * @param em
     * @param user
     * @param group
     * @return
     */
    public User addGroupToUser(User user, Group group) {
        EntityManager em = getEMF().createEntityManager();

        try {
            User userWC = em.find(User.class, user.getId());
            Group groupWC = em.find(Group.class, group.getId());

            if (userWC == null) {
                throw new IllegalStateException("User.addGroupToUser:: User does not found in this context: " + user);
            }
            if (groupWC == null) {
                throw new IllegalStateException("User.addGroupToUser:: Group not found in this context: " + group);
            }

            return userWC.addGroup(groupWC);
        } finally {
            em.close();
        }
    }

    /**
     * 
     * @param em
     * @param user
     * @param group
     * @return
     */
    public User removeGroupFromUser(User user, Group group) {
        EntityManager em = getEMF().createEntityManager();

        try {
            User userWC = em.find(User.class, user.getId());
            Group groupWC = em.find(Group.class, group.getId());

            if (userWC == null) {
                throw new IllegalStateException("User.addGroupToUser:: User does not found in this context: " + user);
            }
            if (groupWC == null) {
                throw new IllegalStateException("User.addGroupToUser:: Group not found in this context: " + group);
            }

            return userWC.addGroup(groupWC);
        } finally {
            em.close();
        }
    }

    /**
     * 
     * @param em
     * @param name
     * @return
     */
    public Group createGroup(String name) {
        EntityManager em = getEMF().createEntityManager();

        try {
            Group group = new Group();
            group.setName(name);

            em.persist(group);

            return group;
        } finally {
            em.close();
        }
    }

    /**
     * 
     * @param em
     * @param group
     */
    public void removeGroup(Group group) {
        EntityManager em = getEMF().createEntityManager();

        Group groupWC = em.find(Group.class, group.getId());

        if (groupWC == null) {
            throw new IllegalStateException("User.removeGroup:: Group not found in this context: " + group);
        }

        int userCount = (Integer) em.createNamedQuery("User.countForGroup").setParameter("GROUP", group).getSingleResult();
        if (userCount > 0) {
            throw new IllegalStateException("User.removeGroup:: Group has users: " + group);
        }

        em.remove(group);
    }

}
