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

import org.eclipse.persistence.config.QueryHints;
import org.eclipse.persistence.expressions.ExpressionBuilder;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.queries.QueryByExamplePolicy;
import org.eclipse.persistence.queries.ReadAllQuery;

import eclipselink.example.jpa.employee.model.Address;
import eclipselink.example.jpa.employee.model.Employee;
import eclipselink.example.jpa.employee.model.Gender;

/**
 * Simple query examples for the XML mapped Employee domain model.
 * 
 * @author dclarke
 * @since EclipseLink 1.1
 */
@SuppressWarnings("unchecked")
public class Queries {

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
    
    public void runExamples(EntityManagerFactory emf) {
        
    }
    
    /**
     * Simple example using JPQL to retrieve all Employee instances sorted by
     * lastName and firstName.
     */
    public List<Employee> readAllEmployeesUsingJPQL(EntityManager em) {
        Query query = em.createQuery("SELECT e FROM Employee e ORDER BY e.id");

        return query.getResultList();
    }

    /**
     * In addition to supporting configuration of JOIN FETCH within the JPQL of
     * a query EclipseLink also supports configuration using query hints. This
     * can be useful as the same named query can be customized for different
     * scenarios in the application as well simplifying trying different query
     * optimizations within the code.
     */
    public List<Employee> joinFetchAndBatchHints(EntityManager em) {
        Query query = em.createQuery("SELECT e FROM Employee e");
        query.setHint(QueryHints.FETCH, "e.address");
        query.setHint(QueryHints.BATCH, "e.phoneNumbers");

        List<Employee> emps = query.getResultList();

        System.out.println(">>> Query Executed... accessing phone numbers of first employee");

        emps.get(0).getPhoneNumbers().size();

        System.out.println(">>> Accessing join/batch fetched attributes for all employees ");
        for (Employee emp : emps) {
            System.out.println("\t> " + emp);
            System.out.println("\t\t " + emp.getAddress());
            System.out.println("\t\t Phone: " + emp.getPhoneNumbers().size());
        }

        return emps;
    }

    public List<Employee> findEmployeesUsingGenderIn(EntityManager em) {
        return em.createQuery("SELECT e FROM Employee e WHERE e.gender IN (:GENDER1, :GENDER2)").setParameter("GENDER1", Gender.Male).setParameter("GENDER2", Gender.Female).getResultList();
    }

    public List<Employee> findUsingNativeReadAllQuery(EntityManager em) {
        ReadAllQuery raq = new ReadAllQuery(Employee.class);
        ExpressionBuilder eb = raq.getExpressionBuilder();
        raq.setSelectionCriteria(eb.get("gender").equal(Gender.Male));

        Query query = JpaHelper.createQuery(raq, em);

        return query.getResultList();
    }

    /**
     * Example of EclipseLink's native query-by-example support. This example
     * created an Employee with a few attributes set and searches for more like
     * it.
     * 
     * @param em
     * @param sampleEmployee
     * @return
     */
    public List<Employee> queryByExample(EntityManager em) {
        Employee sampleEmployee = new Employee();
        sampleEmployee.setGender(Gender.Male);
        sampleEmployee.setAddress(new Address());
        sampleEmployee.getAddress().setCity("Ottawa");

        QueryByExamplePolicy policy = new QueryByExamplePolicy();
        policy.excludeDefaultPrimitiveValues();
        ReadAllQuery roq = new ReadAllQuery(sampleEmployee, policy);

        // Wrap the native query in a JPA Query and execute it.
        Query query = JpaHelper.createQuery(roq, em);

        return (List<Employee>) query.getResultList();
    }

}
