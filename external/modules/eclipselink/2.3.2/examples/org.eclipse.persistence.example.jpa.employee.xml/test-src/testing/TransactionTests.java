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
 * 		dclarke - initial JPA Employee example using XML (bug 217884)
 ******************************************************************************/
package testing;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertNotNull;
import static junit.framework.Assert.assertTrue;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

import model.Employee;

import org.junit.Test;

import example.Queries;
import example.Sample;
import example.Transactions;

@PersistenceContext(unitName = "employee-xml")
public class TransactionTests extends EclipseLinkJPATest {

    Transactions transactions = new Transactions();

    @Test
    public void pessimisticLocking() throws Exception {
        transactions.pessimisticLocking(getEntityManager());
    }

    @Test
    public void updateEmployeeWithCity() throws Exception {
        EntityManager em = getEntityManager();

        transactions.updateEmployeeWithCity(em);
    }

    @Test
    public void createUsingPersist() throws Exception {
        EntityManager em = getEntityManager();

        em.getTransaction().begin();

        Employee emp = transactions.createUsingPersist(em);

        assertNotNull(emp);
        assertTrue(emp.getId() > 0);

        em.getTransaction().rollback();

        Sample.population.verifyCounts(em);
    }

    @Test
    public void createUsingMerge() throws Exception {
        EntityManager em = getEntityManager();

        em.getTransaction().begin();

        Employee emp = transactions.createUsingMerge(em);

        assertNotNull(emp);
        assertTrue(emp.getId() > 0);

        em.getTransaction().rollback();

        Sample.population.verifyCounts(em);
    }

    @Test
    public void mergeDetached() throws Exception {
        EntityManager em = getEntityManager();

        int minId = Queries.minimumEmployeeId(em);
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

    @Test
    public void loadGroupSerializeMerge() {
        EntityManager em = getEntityManager();

        em.getTransaction().begin();

        Employee emp = this.transactions.loadGroupSerializeMerge(em);

        assertEquals(4, getQuerySQLTracker(em).getTotalSQLSELECTCalls());

        assertEquals(1.0, emp.getSalary());

        assertEquals(4, getQuerySQLTracker(em).getTotalSQLSELECTCalls());
        assertEquals(2, getQuerySQLTracker(em).getTotalSQLUPDATECalls());
        assertEquals(1, getQuerySQLTracker(em).getTotalSQLINSERTCalls());

        assertEquals(1.0, emp.getSalary());
        emp.getGender();

        assertEquals(4, getQuerySQLTracker(em).getTotalSQLSELECTCalls());

        em.getTransaction().rollback();
    }

    @Test
    public void copyGroupMerge() {
        EntityManager em = getEntityManager();

        em.getTransaction().begin();

        Employee emp = this.transactions.copyGroupMerge(em);

        assertEquals(4, getQuerySQLTracker(em).getTotalSQLSELECTCalls());

        assertEquals(1.0, emp.getSalary());

        assertEquals(4, getQuerySQLTracker(em).getTotalSQLSELECTCalls());
        assertEquals(2, getQuerySQLTracker(em).getTotalSQLUPDATECalls());
        assertEquals(1, getQuerySQLTracker(em).getTotalSQLINSERTCalls());

        assertEquals(1.0, emp.getSalary());

        assertEquals(4, getQuerySQLTracker(em).getTotalSQLSELECTCalls());

        em.getTransaction().rollback();
    }
}
