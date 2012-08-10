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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import javax.persistence.TypedQuery;

import junit.framework.Assert;

import org.eclipse.persistence.config.QueryHints;
import org.eclipse.persistence.internal.sessions.RepeatableWriteUnitOfWork;
import org.eclipse.persistence.queries.FetchGroup;
import org.eclipse.persistence.sessions.server.ServerSession;
import org.junit.Test;

import testing.util.EclipseLinkJPATest;
import eclipselink.example.jpa.employee.model.Address;
import eclipselink.example.jpa.employee.model.Employee;
import eclipselink.example.jpa.employee.model.User;
import eclipselink.example.jpa.employee.model.util.Sample;
import example.Queries;

/**
 * Simple query examples for the XML mapped Employee domain model.
 * 
 * @author dclarke
 * @since EclipseLink 1.2
 */
@SuppressWarnings("unchecked")
@PersistenceContext(unitName = "employee")
public class QueryTests extends EclipseLinkJPATest {

    private Queries examples = new Queries();

    private Sample population = null;

    public Queries getExamples() {
        return this.examples;
    }

    @Test
    public void readAllEmployees_JPQL() {
        EntityManager em = getEntityManager("u1");

        List<Employee> emps = getExamples().readAllEmployeesUsingJPQL(em);

        this.population.verifySame(emps);
    }

    @Test
    public void joinFetchAndBatchHints() {
        EntityManager em = getEntityManager("u1");

        List<Employee> emps = getExamples().joinFetchAndBatchHints(em);

        assertNotNull(emps);

        assertEquals(2, getQuerySQLTracker(em).getTotalSQLSELECTCalls());
    }

    @Test
    public void testGenderIn() throws Exception {
        List<Employee> emps = getExamples().findEmployeesUsingGenderIn(getEntityManager("u1"));

        assertNotNull(emps);
    }

    /**
     * Test a dynamic JPQL query comparing a value with the custom Enum
     * converter (Gender).
     */
    @Test
    public void testGenderEquals() throws Exception {
        EntityManager em = getEntityManager("u1");

        List<Employee> emps = em.createQuery("SELECT e FROM Employee e WHERE e.gender = eclipselink.example.jpa.employee.model.Gender.Male").getResultList();

        assertNotNull(emps);
    }

    @Test
    public void testReadAllExressions() throws Exception {
        List<Employee> emps = getExamples().findUsingNativeReadAllQuery(getEntityManager("u1"));

        assertNotNull(emps);
    }

    @Test
    public void updateAllWithSubSelect() throws Exception {
        EntityManager em = getEntityManager("u1");

        Address minAddress = (Address) em.createQuery("SELECT a FROM Address a where a.id in (SELECT MIN(aa.id) FROM Address aa)").getSingleResult();
        Assert.assertNotNull(minAddress);

        em.getTransaction().begin();
        em.createQuery("UPDATE Employee e SET e.address = :ADDR WHERE e.address IS NULL").setParameter("ADDR", minAddress).executeUpdate();
        em.getTransaction().rollback();
    }

    @Test
    public void queryByExampleTest() throws Exception {
        EntityManager em = getEntityManager("u1");

        List<Employee> emps = this.examples.queryByExample(em);

        assertNotNull(emps);

        // assertEquals(minEmp.getId(), emp.getId());
    }

    @Test
    public void findEmployeeWithPhoneTypeIN() {
        EntityManager em = getEntityManager("u1");

        Query query = em.createQuery("SELECT DISTINCT(e) FROM Employee e JOIN e.phoneNumbers p WHERE p.type IN :TYPES");

        Collection<String> types = new ArrayList<String>();
        types.add("WORK");
        types.add("CELL");

        query.setParameter("TYPES", types);

        List<Employee> emps = query.getResultList();

        for (Employee result : emps) {
            System.out.println(result);
        }
    }

    @Test
    public void attributeGroupExample() {
        EntityManager em = getEntityManager("u2");

        FetchGroup fg = new FetchGroup();
        fg.addAttribute("firstName");
        fg.addAttribute("lastName");
        fg.addAttribute("salary");
        fg.addAttribute("address");
        fg.setShouldLoad(true);
        
        TypedQuery<Employee> query = em.createNamedQuery("Employee.findAll", Employee.class);
        query.setHint(QueryHints.FETCH_GROUP, fg);
        
        List<Employee> emps =query.getResultList();
        
        for (Employee emp: emps) {
            System.out.println("> " + emp.getFirstName() + " $" + emp.getSalary());
            emp.getAddress();
        }
    }

    @Test
    public void populateRelationshipWithinTransaction() {
        EntityManager em = getEntityManager("u1");
        
        em.getEntityManagerFactory().getCache().evictAll();
        
        TypedQuery<Employee> query = em.createQuery("SELECT e FROM Employee e WHERE e.address IS NOT NULL", Employee.class);
        query.setMaxResults(1);
        
        Employee emp = query.getSingleResult();
        
        assertNotNull(emp);
        
        em.unwrap(RepeatableWriteUnitOfWork.class).beginEarlyTransaction();
        
        Address address = emp.getAddress();
        
        assertNotNull(address);
        
        ServerSession server =em.unwrap(ServerSession.class);
        
        assertEquals(1, server.getIdentityMapAccessorInstance().getIdentityMap(Employee.class).getSize());
        assertEquals(0, server.getIdentityMapAccessorInstance().getIdentityMap(Address.class).getSize());
        
        Employee sharedEmployee = (Employee) server.getIdentityMapAccessor().getFromIdentityMap(emp);
        
        assertNotNull(sharedEmployee);
        
    }
    
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
