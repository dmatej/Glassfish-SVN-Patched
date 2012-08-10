/*******************************************************************************
 * Copyright (c) 1998, 2010 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 * 		dclarke - initial JPA Employee example using XML (bug 217884)
 ******************************************************************************/
package testing.model;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertNotNull;
import static junit.framework.Assert.assertTrue;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

import org.junit.Test;

import testing.util.EclipseLinkJPATest;
import eclipselink.example.jpa.employee.model.Employee;
import eclipselink.example.jpa.employee.model.User;
import eclipselink.example.jpa.employee.model.util.Sample;
import example.Transactions;

@PersistenceContext(unitName = "employee")
public class TransactionTests extends EclipseLinkJPATest {

    Transactions transactions = new Transactions();

    private Sample population;

    @Test
    public void pessimisticLocking() throws Exception {
        transactions.pessimisticLocking(getEntityManager("u1"));
    }

    @Test
    public void updateEmployeeWithCity() throws Exception {
        EntityManager em = getEntityManager("u1");

        transactions.updateEmployeeWithCity(em);
    }

    @Test
    public void createUsingPersist() throws Exception {
        EntityManager em = getEntityManager("u1");

        em.getTransaction().begin();

        Employee emp = transactions.createUsingPersist(em);

        assertNotNull(emp);
        assertTrue(emp.getId() > 0);

        em.getTransaction().rollback();

        // this.population.verifyCounts(em);
    }

    @Test
    public void createUsingMerge() throws Exception {
        EntityManager em = getEntityManager("u1");

        em.getTransaction().begin();

        Employee emp = transactions.createUsingMerge(em);

        assertNotNull(emp);
        assertTrue(emp.getId() > 0);

        em.getTransaction().rollback();

        // this.population.verifyCounts(em);
    }

    @Test
    public void addAttribute() {
        EntityManager em = getEntityManager("u1");

        em.getTransaction().begin();

        this.transactions.addAttribute(em);

        assertEquals(3, getQuerySQLTracker(em).getTotalSQLSELECTCalls());
        assertEquals(1, getQuerySQLTracker(em).getTotalSQLINSERTCalls());
        assertEquals(1, getQuerySQLTracker(em).getTotalSQLUPDATECalls());
        assertEquals(0, getQuerySQLTracker(em).getTotalSQLDELETECalls());

        em.getTransaction().rollback();
    }

    @Test
    public void mergeDetached() throws Exception {
        EntityManager em = getEntityManager("u1");

        int minId = ((Number) em.createQuery("SELECT MIN(e.id) FROM Employee e").getSingleResult()).intValue();
        Employee emp = em.find(Employee.class, minId);
        assertNotNull(emp);

        emp.setSalary(emp.getSalary() + 1);

        em.getTransaction().begin();

        getQuerySQLTracker(em).reset();
        assertEquals(0, getQuerySQLTracker(em).getTotalSQLUPDATECalls());

        em.merge(emp);

        em.flush();
        assertEquals(2, getQuerySQLTracker(em).getTotalSQLUPDATECalls());

    }

    /**
     * TODO
     * 
     * @see testing.util.EclipseLinkJPATest#getEntityManager(java.lang.String,
     *      java.lang.String)
     */
    protected EntityManager getEntityManager(String userid) {
        EntityManager em = super.getEntityManager(userid, "password");

        User user = getCurrentUser();

        getQuerySQLTracker(em).reset();

        if (this.population == null || !this.population.getCompany().getCode().equals(user.getCompany().getCode())) {
            this.population = new Sample(user.getCompany());
        }

        return em;
    }

}
