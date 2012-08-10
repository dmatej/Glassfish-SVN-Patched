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
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;

import model.*;

import org.eclipse.persistence.config.QueryHints;
import org.eclipse.persistence.expressions.ExpressionBuilder;
import org.eclipse.persistence.jpa.JpaEntityManager;
import org.eclipse.persistence.queries.*;

/**
 * Simple query examples.
 */
public class Queries {

    public static void main(String[] args) throws Exception {        
        EntityManagerFactory emf = Persistence.createEntityManagerFactory("employee");
        Populate.populate(emf);        
        Queries queries = new Queries();
        queries.clear(emf);
        queries.readAllEmployees(emf);
        queries.joinFetch(emf);
        queries.joinFetchHint(emf);
        queries.minimumEmployeeId(emf);
        queries.findEmployeesUsingGenderIn(emf);
        queries.findUsingNativeReadAllQuery(emf);
        queries.queryByExample(emf);
        emf.close();
    }

    public List<Employee> readAllEmployees(EntityManagerFactory emf) {
        System.out.println("Reading all employees.");
        
        EntityManager em = emf.createEntityManager();
        
        CriteriaBuilder cb = em.getCriteriaBuilder();
        CriteriaQuery criteria = cb.createQuery(Employee.class);
        Root<Employee> entity = criteria.from(Employee.class);
        criteria.orderBy(cb.asc(entity.get("lastName")), cb.asc(entity.get("firstName")));
        Query query = em.createQuery(criteria);
        
        List<Employee> result = query.getResultList();
        em.close();
        return result;
    }

    public List<Employee> joinFetch(EntityManagerFactory emf) {
        System.out.println("Reading all employees and their address.");
        
        EntityManager em = emf.createEntityManager();
        
        CriteriaBuilder cb = em.getCriteriaBuilder();
        CriteriaQuery criteria = cb.createQuery(Employee.class);
        Root<Employee> entity = criteria.from(Employee.class);
        entity.fetch("address");
        criteria.orderBy(cb.asc(entity.get("lastName")), cb.asc(entity.get("firstName")));
        Query query = em.createQuery(criteria);

        List<Employee> result = query.getResultList();
        em.close();
        return result;
    }

    public List<Employee> joinFetchHint(EntityManagerFactory emf) {
        System.out.println("Reading all employees and their address, manager, manager's address, and manager's phones.");
        
        EntityManager em = emf.createEntityManager();
        
        CriteriaBuilder cb = em.getCriteriaBuilder();
        CriteriaQuery criteria = cb.createQuery(Employee.class);
        Root<Employee> entity = criteria.from(Employee.class);
        criteria.where(cb.equal(entity.get("address").get("city"), "Ottawa"));
        criteria.orderBy(cb.asc(entity.get("lastName")), cb.asc(entity.get("firstName")));
        Query query = em.createQuery(criteria);
        
        // TODO: Remove this workaround to prepare the query once the setHint bug is fixed.
        query.getResultList();
        
        query.setHint(QueryHints.FETCH, "e.address");
        query.setHint(QueryHints.FETCH, "e.manager");
        query.setHint(QueryHints.FETCH, "e.manager.address");
        query.setHint(QueryHints.BATCH, "e.manager.phoneNumbers");
        List<Employee> emps = query.getResultList();

        for (Employee emp : emps) {
            emp.getManager().getPhoneNumbers().size();
        }

        em.close();
        return emps;
    }

    public int minimumEmployeeId(EntityManagerFactory emf) {
        System.out.println("Reading the minimum employee id.");
        
        EntityManager em = emf.createEntityManager();
        
        CriteriaBuilder cb = em.getCriteriaBuilder();
        CriteriaQuery criteria = cb.createQuery(Employee.class);
        Root<Employee> entity = criteria.from(Employee.class);
        criteria.select(cb.min(entity.<Number>get("id")));
        Query query = em.createQuery(criteria);
        
        int result = ((Number)query.getSingleResult()).intValue();
        em.close();
        return result;
    }

    public List<Employee> findEmployeesUsingGenderIn(EntityManagerFactory emf) {
        System.out.println("Reading employees by gender.");
        
        EntityManager em = emf.createEntityManager();

        CriteriaBuilder cb = em.getCriteriaBuilder();
        CriteriaQuery criteria = cb.createQuery(Employee.class);
        Root<Employee> entity = criteria.from(Employee.class);
        criteria.where(entity.get("gender").in(Gender.Male, Gender.Female));
        Query query = em.createQuery(criteria);

        List<Employee> result = query.getResultList();
        em.close();
        return result;
    }

    public List<Employee> findUsingNativeReadAllQuery(EntityManagerFactory emf) {
        System.out.println("Reading employees by gender using native API.");
        
        EntityManager em = emf.createEntityManager();
        
        ReadAllQuery query = new ReadAllQuery(Employee.class);
        ExpressionBuilder eb = query.getExpressionBuilder();
        query.setSelectionCriteria(eb.get("gender").equal(Gender.Male));

        List<Employee> result = em.unwrap(JpaEntityManager.class).createQuery(query).getResultList();
        em.close();
        return result;
    }

    /**
     * Example of EclipseLink's native query-by-example support.
     * 
     * @param em
     * @param sampleEmployee
     * @return
     */
    public Employee queryByExample(EntityManagerFactory emf) {
        System.out.println("Reading employee using query by example.");
        
        EntityManager em = emf.createEntityManager();
        
        Employee sampleEmployee = new Employee();
        sampleEmployee.setFirstName("Fred");
        sampleEmployee.setLastName("Jones");
        sampleEmployee.setGender(Gender.Male);
        Address sampleAddress = new Address();
        sampleAddress.setCity("Victoria");
        sampleEmployee.setAddress(sampleAddress);
        
        Employee result = (Employee) em.unwrap(JpaEntityManager.class).createQueryByExample(sampleEmployee).getSingleResult();
        em.close();
        return result;
    }
    
    public void clear(EntityManagerFactory factory) {
        System.out.println("Clearing cache.");
        factory.getCache().evictAll();        
    }
}
