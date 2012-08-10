/*******************************************************************************
 * Copyright (c) 1998, 2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *              dclarke - initial JPA Employee example using XML (bug 217884)
 ******************************************************************************/
package example;

import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.persistence.Query;

import model.*;

import org.eclipse.persistence.config.PessimisticLock;
import org.eclipse.persistence.config.QueryHints;

public class Transactions {

    public static void main(String[] args) throws Exception {
        EntityManagerFactory emf = Persistence.createEntityManagerFactory("employee");
        Transactions transactions = new Transactions();
        transactions.createUsingPersist(emf);
        transactions.createUsingMerge(emf);
        transactions.pessimisticLocking(emf);
        transactions.updateEmployeeWithCity(emf);
        emf.close();
    }
    
    /**
     * New entities with new related related entities can be persisted using
     * <code>EntityManager.persist(newEntity)</code>. The cascade setting on the
     * mappings determine how the related entities are handled. In this case
     * Employee has its relationship to Address and PhoneNumber configured with
     * cascade-all so the associated new entities will also be persisted.
     */
    public Employee createUsingPersist(EntityManagerFactory emf) {
        System.out.println("Creating new employee using persist.");
        
        EntityManager em = emf.createEntityManager();
        
        Employee emp = new Employee();
        emp.setFirstName("Sample");
        emp.setLastName("Employee");
        emp.setGender(Gender.Male);
        emp.setSalary(123456);

        Address address = new Address();
        emp.setAddress(address);

        emp.addPhoneNumber("Mobile", "613", "555-1212");

        em.getTransaction().begin();
        em.persist(emp);
        em.getTransaction().commit();
        em.close();

        return emp;
    }

    public Employee createUsingMerge(EntityManagerFactory emf) {
        System.out.println("Creating new employee using merge.");
        
        EntityManager em = emf.createEntityManager();
        
        Employee emp = new Employee();
        emp.setFirstName("Sample");
        emp.setLastName("Employee");
        emp.setGender(Gender.Male);
        emp.setSalary(123456);

        Address address = new Address();
        emp.setAddress(address);

        emp.addPhoneNumber("Mobile", "613", "555-1212");

        em.getTransaction().begin();
        // When merging the managed instance is returned from the call.
        // Further usage within the transaction must be done with this managed
        // entity.
        emp = em.merge(emp);
        em.getTransaction().commit();
        em.close();

        return emp;
    }

    /**
     * 
     * @param em
     * @throws Exception
     */
    public void pessimisticLocking(EntityManagerFactory emf) throws Exception {
        System.out.println("Using pessimistic locking on employee.");
        
        EntityManager em = emf.createEntityManager();

        // Find any Employee id
        Long minId = (Long)em.createQuery("SELECT e.id FROM Employee e").getResultList().get(0);

        em.getTransaction().begin();

        // Lock Employee using query with hint
        Query query = em.createQuery("SELECT e FROM Employee e WHERE e.id = :ID");
        query.setParameter("ID", minId);
        query.setHint(QueryHints.PESSIMISTIC_LOCK, PessimisticLock.Lock);        
        Employee emp = (Employee)query.getSingleResult();

        emp.setSalary(emp.getSalary() - 1);

        em.getTransaction().commit();
        em.close();
    }

    /**
     * This example illustrates the use of a query returning an entity and data
     * from a related entity within a transaction. The returned entities are
     * managed and thus any changes are reflected in the database upon flush.
     * 
     * @param em
     * @throws Exception
     */
    public void updateEmployeeWithCity(EntityManagerFactory emf) throws Exception {
        System.out.println("Querying employee and its address' city and updating the employee's salary.");
        
        EntityManager em = emf.createEntityManager();
        
        em.getTransaction().begin();

        List<Object[]> emps = em.createQuery("SELECT e, e.address.city FROM Employee e").getResultList();
        Employee emp = (Employee) emps.get(0)[0];
        emp.setSalary(emp.getSalary() + 1);

        em.getTransaction().commit();
        em.close();
    }

}
