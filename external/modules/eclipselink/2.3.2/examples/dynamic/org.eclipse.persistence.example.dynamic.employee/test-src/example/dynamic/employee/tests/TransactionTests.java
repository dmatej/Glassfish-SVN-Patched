package example.dynamic.employee.tests;

import java.util.Collection;
import java.util.Date;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;

import junit.framework.Assert;

import org.eclipse.persistence.descriptors.changetracking.ChangeTracker;
import org.eclipse.persistence.dynamic.DynamicClassLoader;
import org.eclipse.persistence.dynamic.DynamicEntity;
import org.eclipse.persistence.dynamic.DynamicType;
import org.eclipse.persistence.internal.descriptors.changetracking.AggregateAttributeChangeListener;
import org.eclipse.persistence.internal.descriptors.changetracking.AttributeChangeListener;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.jpa.dynamic.JPADynamicHelper;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import testing.QuerySQLTracker;
import example.EmployeeDynamicMappings;
import example.Main;

public class TransactionTests {

    private static EntityManagerFactory emf = null;

    @Test
    public void testMultipleTransactionsModifyNewInstance() {
        EntityManager em = emf.createEntityManager();
        JPADynamicHelper helper = new JPADynamicHelper(em);

        DynamicEntity newEmp = helper.newDynamicEntity("Employee");
        newEmp.set("firstName", "Delete");
        newEmp.set("lastName", "Delete");
        newEmp.set("gender", "Male");

        em.getTransaction().begin();
        em.persist(newEmp);
        em.getTransaction().commit();
        
        //Assert.assertEquals(2, getQuerySQLTracker(em).getTotalSQLINSERTCalls());
        //Assert.assertEquals(1, getQuerySQLTracker(em).getTotalSQLUPDATECalls());
        //Assert.assertEquals(1, getQuerySQLTracker(em).getTotalSQLSELECTCalls());

        Assert.assertTrue(newEmp instanceof ChangeTracker);
        Assert.assertNotNull(((ChangeTracker) newEmp)._persistence_getPropertyChangeListener());
        Assert.assertTrue(((ChangeTracker) newEmp)._persistence_getPropertyChangeListener() instanceof AttributeChangeListener);

        newEmp.set("salary", 1);

        DynamicEntity period = helper.newDynamicEntity("EmploymentPeriod");
        period.set("startDate", new Date(System.currentTimeMillis()));
        newEmp.set("period", period);

        DynamicEntity newAddress = helper.newDynamicEntity("Address");
        newEmp.set("address", newAddress);

        DynamicEntity newPhone = helper.newDynamicEntity("PhoneNumber");
        newPhone.set("type", "DELETE");
        newPhone.set("areaCode", "650");
        newPhone.set("number", "6135552222");
        newPhone.set("owner", newEmp);
        newEmp.<Collection<DynamicEntity>> get("phoneNumbers").add(newPhone);

        em.getTransaction().begin();
        em.persist(newPhone);
        em.persist(newAddress);
        em.getTransaction().commit();

        //Assert.assertEquals(4, getQuerySQLTracker(em).getTotalSQLINSERTCalls());
        //Assert.assertEquals(4, getQuerySQLTracker(em).getTotalSQLUPDATECalls());
        //Assert.assertEquals(2, getQuerySQLTracker(em).getTotalSQLSELECTCalls());

        Assert.assertTrue(newPhone instanceof ChangeTracker);
        Assert.assertNotNull(((ChangeTracker) newPhone)._persistence_getPropertyChangeListener());
        Assert.assertTrue(((ChangeTracker) newPhone)._persistence_getPropertyChangeListener() instanceof AttributeChangeListener);

        Assert.assertTrue(newAddress instanceof ChangeTracker);
        Assert.assertNotNull(((ChangeTracker) newAddress)._persistence_getPropertyChangeListener());
        Assert.assertTrue(((ChangeTracker) newAddress)._persistence_getPropertyChangeListener() instanceof AttributeChangeListener);

        Assert.assertTrue(period instanceof ChangeTracker);
        Assert.assertNotNull(((ChangeTracker) period)._persistence_getPropertyChangeListener());
        Assert.assertTrue(((ChangeTracker) period)._persistence_getPropertyChangeListener() instanceof AggregateAttributeChangeListener);

        newPhone.set("areaCode", "666");
        period.set("endDate", new Date(System.currentTimeMillis() + 100000));
        newAddress.set("city", "Ottawa");

        em.getTransaction().begin();
        em.getTransaction().commit();

        //Assert.assertEquals(4, getQuerySQLTracker(em).getTotalSQLINSERTCalls());
        //Assert.assertEquals(7, getQuerySQLTracker(em).getTotalSQLUPDATECalls());
        //Assert.assertEquals(2, getQuerySQLTracker(em).getTotalSQLSELECTCalls());
    }

    protected QuerySQLTracker getQuerySQLTracker(EntityManager em) {
        return QuerySQLTracker.getTracker(JpaHelper.getEntityManager(em).getActiveSession());
    }

    /*
    @BeforeClass
    public static void createEMF() {
        // Create a dynamic class loader and create the types.
        DynamicClassLoader dcl = new DynamicClassLoader(Thread.currentThread().getContextClassLoader());
        List<DynamicType> types = EmployeeDynamicMappings.createTypes(dcl, "example.jpa.dynamic.model.employee");        
        
        // Create an entity manager factory.
        emf = Main.createEntityManagerFactory(dcl, "default");
        
        // Create JPA Dynamic Helper (with the emf above) and after the types
        // have been created and add the types through the helper.
        JPADynamicHelper helper = new JPADynamicHelper(emf);
        helper.addTypes(true, true, types);
        
        QuerySQLTracker.install(JpaHelper.getServerSession(emf));
    }
    */
    
    @BeforeClass
    public static void createEMF() {
        // Create an entity manager factory.
        emf = Main.createEntityManagerFactory(new DynamicClassLoader(Thread.currentThread().getContextClassLoader()), "jpa");
        QuerySQLTracker.install(JpaHelper.getServerSession(emf));
    }
    
    @AfterClass
    public static void closeEMF() {
        emf.close();
    }

    @After
    public void deleteTestData() {
        EntityManager em = emf.createEntityManager();
        try {
            em.getTransaction().begin();
            em.createQuery("DELETE FROM PhoneNumber p WHERE p.owner.firstName = 'Delete' OR p.owner.lastName = 'Delete'").executeUpdate();
            em.createQuery("DELETE FROM Employee e WHERE e.firstName = 'Delete' OR e.lastName = 'Delete'").executeUpdate();
            em.getTransaction().commit();
        } finally {
            if (em.getTransaction().isActive()) {
                em.getTransaction().rollback();
            }
            em.close();
            QuerySQLTracker.getTracker(JpaHelper.getServerSession(emf)).reset();
        }
    }
}
