package example;

import javax.persistence.*;

public class JavaSEExample {

    public static void main(String[] args) {
        EntityManagerFactory emf = Persistence.createEntityManagerFactory("employee");

        EntityManager em = emf.createEntityManager();

        em.createQuery("SELECT e FROM Employee e").getResultList();

        em.getTransaction().begin();

        //em.createNamedQuery("Employee.findMin").setLockMode(LockModeType.OPTIMISTIC_FORCE_INCREMENT).getSingleResult();

        em.getTransaction().commit();
        em.close();
        emf.close();
    }
}
