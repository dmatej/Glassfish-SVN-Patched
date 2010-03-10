package example;

import java.util.HashMap;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import example.util.ExamplePropertiesLoader;

public class JavaSEExample {

    public static void main(String[] args) {
        Map<String, Object> properties = new HashMap<String, Object>();
        ExamplePropertiesLoader.loadProperties(properties);
        EntityManagerFactory emf = Persistence.createEntityManagerFactory("employee", properties);

        EntityManager em = emf.createEntityManager();

        em.createQuery("SELECT e FROM Employee e").getResultList();

        em.getTransaction().begin();

        //em.createNamedQuery("Employee.findMin").setLockMode(LockModeType.OPTIMISTIC_FORCE_INCREMENT).getSingleResult();

        em.getTransaction().commit();
        em.close();
        emf.close();
    }
}
