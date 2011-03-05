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

        try {
            
            // Transactions examples
            Transactions transactions = new Transactions();
            
            // Create using persist
            em.getTransaction().begin();
            transactions.createUsingPersist(em);
            em.getTransaction().rollback();
            
            // Create using merge
            em.getTransaction().begin();
            transactions.createUsingMerge(em);
            em.getTransaction().rollback();

            // Partial Merge
            em.getTransaction().begin();
            transactions.partialMerge(em);
            em.getTransaction().rollback();
            em.clear();
            emf.getCache().evictAll();
            
        } finally {
            em.close();
            emf.close();
        }
    }
    
}
