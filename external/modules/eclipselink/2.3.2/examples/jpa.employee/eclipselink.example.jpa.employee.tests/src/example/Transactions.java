/*******************************************************************************
 * Copyright (c) 2007, 2010 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *      dclarke - JPA Employee example using XML (bug 217884)
 ******************************************************************************/
package example;

import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Query;

import org.eclipse.persistence.config.PessimisticLock;
import org.eclipse.persistence.config.QueryHints;

import eclipselink.example.jpa.employee.model.Address;
import eclipselink.example.jpa.employee.model.Employee;
import eclipselink.example.jpa.employee.model.Gender;

@SuppressWarnings("unchecked")
public class Transactions {

    /**
     * TODO
     */
    public static void main(String[] args) {
        EntityManagerFactory emf = Main.createEMF(null);
        try {
            new Queries().runExamples(emf);
        } finally {
            emf.close();
        }
    }

    /**
     * TODO
     */
    public void runExamples(EntityManagerFactory emf) {
        EntityManager em = null;

        try {
            // Transactions examples
            Transactions transactions = new Transactions();

            // Create using persist
            em = emf.createEntityManager();
            em.getTransaction().begin();
            transactions.createUsingPersist(em);
            em.getTransaction().rollback();
            em.close();

            // Create using merge
            em = emf.createEntityManager();
            em.getTransaction().begin();
            transactions.createUsingMerge(em);
            em.getTransaction().rollback();
            em.close();

        } finally {
            if (em != null) {
                if (em.getTransaction().isActive()) {
                    em.getTransaction().rollback();
                }
                if (em.isOpen()) {
                    em.close();
                }
            }
        }
    }

    /**
     * New entities with new related related entities can be persisted using
     * <code>EntityManager.persist(newEntity)</code>. The cascade setting on the
     * mappings determine how the related entities are handled. In this case
     * Employee has its relationship to Address and PhoneNumber configured with
     * cascade-all so the associated new entities will also be persisted.
     */
    public Employee createUsingPersist(EntityManager em) {
        Employee emp = new Employee();
        emp.setFirstName("Sample");
        emp.setLastName("Employee");
        emp.setGender(Gender.Male);
        emp.setSalary(123456);

        Address address = new Address();
        emp.setAddress(address);

        emp.addPhoneNumber("Mobile", "613", "555-1212");

        em.persist(emp);

        em.flush();

        return emp;
    }

    /**
	 * 
	 */
    public Employee createUsingMerge(EntityManager em) {
        Employee emp = new Employee();
        emp.setFirstName("Sample");
        emp.setLastName("Employee");
        emp.setGender(Gender.Male);
        emp.setSalary(123456);

        Address address = new Address();
        emp.setAddress(address);

        emp.addPhoneNumber("Mobile", "613", "555-1212");

        // When merging the managed instance is returned from the call.
        // Further usage within the transaction must be done with this managed
        // entity.
        emp = em.merge(emp);

        em.flush();

        return emp;
    }

    /**
     * 
     * @param em
     * @throws Exception
     */
    public void pessimisticLocking(EntityManager em) throws Exception {

        // Find the Employee with the minimum ID
        int minId = ((Number) em.createQuery("SELECT MIN(e.id) FROM Employee e").getSingleResult()).intValue();

        em.getTransaction().begin();

        // Lock Employee using query with hint
        Employee emp = (Employee) em.createQuery("SELECT e FROM Employee e WHERE e.id = :ID").setParameter("ID", minId).setHint(QueryHints.PESSIMISTIC_LOCK, PessimisticLock.Lock).getSingleResult();

        emp.setSalary(emp.getSalary() - 1);

        em.flush();
    }

    /**
     * This example illustrates the use of a query returning an entity and data
     * from a related entity within a transaction. The returned entities are
     * managed and thus any changes are reflected in the database upon flush.
     * 
     * @param em
     * @throws Exception
     */
    public void updateEmployeeWithCity(EntityManager em) throws Exception {
        em.getTransaction().begin();

        List<Object[]> emps = em.createQuery("SELECT e, e.address.city FROM Employee e").getResultList();
        Employee emp = (Employee) emps.get(0)[0];
        emp.setSalary(emp.getSalary() + 1);

        em.flush();

        em.getTransaction().rollback();
    }

    /**
     * TODO
     */
    public Employee addAttribute(EntityManager em) {
        Query query = em.createQuery("SELECT e FROM Employee e WHERE e.id IN (SELECT MIN(ee.id) FROM Employee ee)");

        Employee emp = (Employee) query.getSingleResult();

        emp.set("now", String.valueOf(System.currentTimeMillis()));

        em.flush();

        return emp;
    }
}
