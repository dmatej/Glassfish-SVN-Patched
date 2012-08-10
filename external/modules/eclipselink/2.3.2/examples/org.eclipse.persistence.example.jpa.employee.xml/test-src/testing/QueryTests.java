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
import static junit.framework.Assert.assertSame;
import static junit.framework.Assert.assertTrue;

import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;

import junit.framework.Assert;
import model.Address;
import model.Employee;
import model.Project;

import org.junit.Test;

import example.Queries;
import example.Sample;

/**
 * Simple query examples for the XML mapped Employee domain model.
 * 
 * @author dclarke
 * @since EclipseLink 1.2
 */
@PersistenceContext(unitName = "employee-xml")
public class QueryTests extends EclipseLinkJPATest {

    private Queries examples = new Queries();

    public Queries getExamples() {
        return this.examples;
    }

    /**
     * Simple example using dynamic JP QL to retrieve all Employee instances
     * sorted by lastName and firstName.
     */
    @Test
    public void readAllEmployees_JPQL() {
        EntityManager em = getEntityManager();

        List<Employee> emps = getExamples().readAllEmployeesUsingJPQL(em);

        Sample.population.verifySame(emps);
    }

    @Test
    public void joinFetchAndBatchHints() {
        EntityManager em = getEntityManager();
        
        List<Employee> emps = getExamples().joinFetchAndBatchHints(em);

        assertNotNull(emps);

        // Should see 1 SELECT for the Employee joined with Adress and 1 for the batched phoneNumber
        assertEquals(2, getQuerySQLTracker(em).getTotalSQLSELECTCalls());
    }

    @Test
    public void minEmployeeId() {
        getExamples();
        int minId = Queries.minimumEmployeeId(getEntityManager());

        assertTrue(minId > 0);
    }

    @Test
    public void testLazyLoading() {
        EntityManager em = getEntityManager();
        int minEmpId = Queries.minimumEmployeeId(em);

        Employee emp = em.find(Employee.class, minEmpId);
        assertNotNull(emp);
    }

    @Test
    public void testGenderIn() throws Exception {
        List<Employee> emps = getExamples().findEmployeesUsingGenderIn(getEntityManager());

        assertNotNull(emps);
    }

    /**
     * Test a dynamic JPQL query comparing a value with the custom Enum
     * converter (Gender).
     */
    @Test
    public void testGenderEquals() throws Exception {
        EntityManager em = getEntityManager();

        List<Employee> emps = em.createQuery("SELECT e FROM Employee e WHERE e.gender = model.Gender.Male").getResultList();

        assertNotNull(emps);
    }

    @Test
    public void testReadAllExressions() throws Exception {
        List<Employee> emps = getExamples().findUsingNativeReadAllQuery(getEntityManager());

        assertNotNull(emps);
    }

    @Test
    public void queryResultCaching() {
        EntityManager em = getEntityManager();

        List<Employee> allEmps = em.createNamedQuery("Employee.findAll").getResultList();

        assertEquals(1, getQuerySQLTracker(em).getTotalSQLSELECTCalls());

        List<Employee> allEmps2 = em.createNamedQuery("Employee.findAll").getResultList();

        assertEquals(1, getQuerySQLTracker(em).getTotalSQLSELECTCalls());

        assertEquals(allEmps.size(), allEmps2.size());
        for (int index = 0; index < allEmps.size(); index++) {
            assertSame(allEmps.get(index), allEmps2.get(index));
        }
    }

    @Test
    public void updateAllWithSubSelect() throws Exception {
        EntityManager em = getEntityManager();

        Address minAddress = (Address) em.createQuery("SELECT a FROM Address a where a.id in (SELECT MIN(aa.id) FROM Address aa)").getSingleResult();
        Assert.assertNotNull(minAddress);

        em.getTransaction().begin();
        em.createQuery("UPDATE Employee e SET e.address = :ADDR WHERE e.address IS NULL").setParameter("ADDR", minAddress).executeUpdate();
        em.getTransaction().rollback();
    }

    @Test
    public void selectIn() throws Exception {
        EntityManager em = getEntityManager();

        int minId = Queries.minimumEmployeeId(em);

        Query query = em.createQuery("SELECT e FROM Employee e WHERE e.id IN (:ID1, :ID2)");
        query.setParameter("ID1", -1);
        query.setParameter("ID2", minId);

        query.getResultList();
    }

    @Test
    public void queryByExampleTest() throws Exception {
        EntityManager em = getEntityManager();

        Employee minEmp = Queries.minimumEmployee(em);
        em.clear();

        Employee example = new Employee();
        example.setFirstName(minEmp.getFirstName());
        example.setLastName(minEmp.getLastName());

        Employee emp = this.examples.queryByExample(em, example);

        assertNotNull(emp);

        assertEquals(minEmp.getId(), emp.getId());
    }

    @Test
    public void simpleCriteria() {

    }

    @Test
    public void findUsingTreatAs() {
        EntityManager em = getEntityManager();
        
        List<Employee> emps = this.examples.findUsingTreatAs(em);
     
        assertNotNull(emps);
        
        for (Employee emp: emps) {
            System.out.println(emp.toString());
            for (Project project: emp.getProjects()) {
                System.out.println("\t> " + project);
            }
        }
    }

    @Test
    public void findUsingTreatAsCriteria() {
        EntityManager em = getEntityManager();
        
        List<Employee> emps = this.examples.findUsingTreatAsCriteria(em);
     
        assertNotNull(emps);
        
        for (Employee emp: emps) {
            System.out.println(emp.toString());
            for (Project project: emp.getProjects()) {
                System.out.println("\t> " + project);
            }
        }
    }
}
