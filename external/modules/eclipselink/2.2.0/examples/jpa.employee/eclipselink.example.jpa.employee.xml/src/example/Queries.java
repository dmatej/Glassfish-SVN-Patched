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
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.Query;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Root;

import model.Employee;
import model.Employee_;
import model.Gender;
import model.LargeProject;
import model.LargeProject_;
import model.Project_;
import model.SmallProject;

import org.eclipse.persistence.config.QueryHints;
import org.eclipse.persistence.expressions.ExpressionBuilder;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.queries.QueryByExamplePolicy;
import org.eclipse.persistence.queries.ReadAllQuery;
import org.eclipse.persistence.queries.ReadObjectQuery;

/**
 * Simple query examples for the XML mapped Employee domain model.
 * 
 * @author dclarke
 * @since EclipseLink 1.1
 */
public class Queries {

    /**
     * Simple example using dynamic JP QL to retrieve all Employee instances
     * sorted by lastName and firstName.
     */
    public List<Employee> readAllEmployeesUsingJPQL(EntityManager em) {
        return em.createQuery("SELECT e FROM Employee e ORDER BY e.lastName ASC, e.firstName ASC", Employee.class).getResultList();
    }

    /**
     * In addition to supporting configuration of JOIN FETCH within the JPQL of
     * a query EclipseLink also supports configuration using query hints. This
     * can be useful as the same named query can be customized for different
     * scenarios in the application as well simplifying trying different query
     * optimisations within the code.
     */
    public List<Employee> joinFetchAndBatchHints(EntityManager em) {
        TypedQuery<Employee> query = em.createQuery("SELECT e FROM Employee e", Employee.class);
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

    /**
     * 
     * @param em
     * @return
     */
    public static int minimumEmployeeId(EntityManager em) {
        return ((Number) em.createQuery("SELECT MIN(e.id) FROM Employee e").getSingleResult()).intValue();
    }

    public static Employee minimumEmployee(EntityManager em) {
        return minimumEmployee(em, null);
    }

    public static Employee minimumEmployee(EntityManager em, Map<String, Object> hints) {
        Query q = em.createQuery("SELECT e FROM Employee e WHERE e.id in (SELECT MIN(ee.id) FROM Employee ee)");
        if (hints != null) {
            for (Map.Entry<String, Object> entry : hints.entrySet()) {
                q.setHint(entry.getKey(), entry.getValue());
            }
        }

        return (Employee) q.getSingleResult();
    }

    public List<Employee> findEmployeesUsingGenderIn(EntityManager em) {
        return em.createQuery("SELECT e FROM Employee e WHERE e.gender IN (:GENDER1, :GENDER2)", Employee.class).setParameter("GENDER1", Gender.Male).setParameter("GENDER2", Gender.Female).getResultList();
    }

    @SuppressWarnings("unchecked")
    public List<Employee> findUsingNativeReadAllQuery(EntityManager em) {
        ReadAllQuery raq = new ReadAllQuery(Employee.class);
        ExpressionBuilder eb = raq.getExpressionBuilder();
        raq.setSelectionCriteria(eb.get("gender").equal(Gender.Male));

        Query query = JpaHelper.createQuery(raq, em);

        return query.getResultList();
    }

    /**
     * Example of EclipseLink's native query-by-example support.
     * 
     * @param em
     * @param sampleEmployee
     * @return
     */
    public Employee queryByExample(EntityManager em, Employee sampleEmployee) {
        QueryByExamplePolicy policy = new QueryByExamplePolicy();
        policy.excludeDefaultPrimitiveValues();
        ReadObjectQuery roq = new ReadObjectQuery(sampleEmployee, policy);

        // Wrap the native query in a JPA Query and execute it.
        Query query = JpaHelper.createQuery(roq, em);

        return (Employee) query.getSingleResult();
    }

    public static Employee minEmployeeWithAddressAndPhones(EntityManager em) {
        return (Employee) em.createQuery("SELECT e FROM Employee e JOIN FETCH e.address WHERE e.id IN (SELECT MIN(p.id) FROM PhoneNumber p)", Employee.class).getSingleResult();
    }

    public Employee minEmployeeWithManagerWithAddress(EntityManager em) {
        List<Employee> emps = em.createQuery("SELECT e FROM Employee e JOIN FETCH e.manager WHERE e.manager.address IS NOT NULL ORDER BY e.id", Employee.class).getResultList();
        return emps.get(0);
    }

    public static int minEmployeeIdWithAddressAndPhones(EntityManager em) {
        return ((Number) em.createQuery("SELECT e.id FROM Employee e JOIN FETCH e.address WHERE e.id IN (SELECT MIN(p.id) FROM PhoneNumber p)").getSingleResult()).intValue();
    }

    /**
     * Example of using EclipseLink's casting in JPQL queries using TREAT AS.
     * This query will find all employees who have a LargeProject with a budget
     * greater then 1000 and at least one SmallProject with a name ending in
     * 'Reporter'.
     * 
     * @since EclipseLink 2.1
     */
    public List<Employee> findUsingTreatAs(EntityManager em) {
        return em.createQuery("SELECT DISTINCT(e) FROM Employee e JOIN TREAT(e.projects AS LargeProject) lp JOIN TREAT(e.projects AS SmallProject) sp WHERE lp.budget > 1000 AND sp.name LIKE '%Reporter'", Employee.class).getResultList();
    }

    /**
     * Example of using EclipseLink's casting in criteria queries using AS. This
     * query will find all employees who have a LargeProject with a budget
     * greater then 1000 and at least one SmallProject with a name ending in
     * 'Reporter'.
     * 
     * @since EclipseLink 2.1
     */
    public List<Employee> findUsingTreatAsCriteria(EntityManager em) {
        CriteriaBuilder qb = em.getCriteriaBuilder();

        CriteriaQuery<Employee> cq = qb.createQuery(Employee.class).distinct(true);
        Root<Employee> empRoot = cq.from(em.getMetamodel().entity(Employee.class));

        Path<Double> largeProjectBudget = ((Path<LargeProject>) empRoot.join(Employee_.projects).as(LargeProject.class)).get(LargeProject_.budget);
        Path<String> smallProjectName = ((Path<SmallProject>) empRoot.join(Employee_.projects).as(SmallProject.class)).get(Project_.name);

        cq.where(qb.and(qb.greaterThan(largeProjectBudget, 1000d), qb.like(smallProjectName, "%Reporter")));

        return em.createQuery(cq).getResultList();
    }
}
