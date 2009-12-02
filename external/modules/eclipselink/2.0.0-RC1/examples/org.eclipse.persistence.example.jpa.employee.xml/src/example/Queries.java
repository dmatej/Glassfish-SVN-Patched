/*******************************************************************************
 * Copyright (c) 1998, 2009 Oracle. All rights reserved.
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
package example;

import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.Query;

import model.Employee;
import model.Gender;

import org.eclipse.persistence.config.QueryHints;
import org.eclipse.persistence.expressions.ExpressionBuilder;
import org.eclipse.persistence.internal.jpa.EJBQueryImpl;
import org.eclipse.persistence.internal.jpa.EntityManagerImpl;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.queries.*;

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
		return em.createQuery("SELECT e FROM Employee e ORDER BY e.lastName ASC, e.firstName ASC").getResultList();
	}

	public List<Employee> joinFetchJPQL(EntityManager em) {
		return em.createQuery("SELECT e FROM Employee e JOIN FETCH e.address ORDER BY e.lastName ASC, e.firstName ASC").getResultList();
	}

	public List<Employee> joinFetchHint(EntityManager em) {
		Query query = em.createQuery("SELECT e FROM Employee e WHERE e.manager.address.city = 'Ottawa' ORDER BY e.lastName ASC, e.firstName ASC");
		query.setHint(QueryHints.FETCH, "e.address");
		query.setHint(QueryHints.FETCH, "e.manager");
		query.setHint(QueryHints.FETCH, "e.manager.address");
		query.setHint(QueryHints.BATCH, "e.manager.phoneNumbers");
		List<Employee> emps = query.getResultList();

		for (Employee emp : emps) {
			emp.getManager().getPhoneNumbers().size();
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
		Query query = new EJBQueryImpl(roq, (EntityManagerImpl) JpaHelper.getEntityManager(em));		
		return (Employee) query.getSingleResult();
	}
	
    public static int minEmployeeIdWithAddressAndPhones(EntityManager em) {
        return (Integer) em.createQuery("SELECT MIN(e.id) FROM Employee e JOIN FETCH e.address WHERE e.id IN (SELECT MIN(p.id) FROM PhoneNumber p)").getSingleResult();
    }

    public Employee minEmployeeWithAddressAndPhones(EntityManager em) {
		return (Employee) em.createQuery("SELECT e FROM Employee e JOIN FETCH e.address WHERE e.id IN (SELECT MIN(p.id) FROM PhoneNumber p)").getSingleResult();
	}
}
