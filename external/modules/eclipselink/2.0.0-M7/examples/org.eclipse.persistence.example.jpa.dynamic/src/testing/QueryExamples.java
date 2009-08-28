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
 * 		dclarke - initial author of demo
 ******************************************************************************/
package testing;

import java.util.List;

import javax.persistence.*;

import junit.framework.Assert;

import org.eclipse.persistence.expressions.ExpressionBuilder;
import org.eclipse.persistence.extension.dynamic.*;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.queries.ReadAllQuery;
import org.junit.Test;

/**
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
@PersistenceContext(unitName = "employee")
public class QueryExamples extends EclipseLinkJPATest {

	/**
	 * Simple example using dynamic JP QL to retrieve all Employee instances
	 * sorted by lastName and firstName.
	 */
	// @Test
	public void readAllEmployees_JPQL() {
		EntityManager em = getEMF().createEntityManager();

		em
				.createQuery(
						"SELECT e FROM Employee e ORDER BY e.lastName ASC, e.firstName ASC")
				.getResultList();

		em.close();
	}

	// @Test
	public void joinFetchJPQL() {
		EntityManager em = getEMF().createEntityManager();

		em
				.createQuery(
						"SELECT e FROM Employee e JOIN FETCH e.address ORDER BY e.lastName ASC, e.firstName ASC")
				.getResultList();

		em.close();
	}

	@Test
	public void rownumExample() {
		EntityManager em = getEMF().createEntityManager();

		Query query = em
				.createQuery("SELECT e FROM Employee e ORDER BY e.lastName ASC, e.firstName ASC");
		query.setFirstResult(5);
		query.setMaxResults(5);
		List<DynamicEntity> emps = query.getResultList();
		
		Assert.assertNotNull(emps);
		Assert.assertFalse(emps.isEmpty());

		em.close();
	}

	@Test
	public void testJPQLOrdering() {
		EntityManager em = getEMF().createEntityManager();

		Query query = em
				.createQuery("SELECT e FROM Employee e");

		JpaHelper.getReadAllQuery(query).addAscendingOrdering("firstName");
		JpaHelper.getReadAllQuery(query).addDescendingOrdering("lastName");

		List<DynamicEntity> emps = query.getResultList();

		Assert.assertNotNull(emps);

		em.close();
	}

	@Test
	public void testFindAll() {
		EntityManager em = getEMF().createEntityManager();

		Query query = em.createQuery("SELECT e FROM Employee e");
		List<DynamicEntity> emps = query.getResultList();

		Assert.assertNotNull(emps);
		Assert.assertFalse(emps.isEmpty());

		em.close();
	}


	@Test
	public void testQueryByExample() throws Exception {
		EntityType type = DynamicJpaHelper.getType(getEMF(), "Employee");
		
		EntityManager em = getEMF().createEntityManager();

		DynamicEntity exampleEmp = (DynamicEntity) type.newInstance();
		exampleEmp.set("firstName", "Doug");
		exampleEmp.set("lastName", "Clarke");

		ReadAllQuery raq = new ReadAllQuery(type.getJavaClass());
		raq.setExampleObject(exampleEmp);

		Query query = JpaHelper.createQuery(raq, em);
		List<DynamicEntity> emps = query.getResultList();

		Assert.assertNotNull(emps);
	}

	@Test
	public void pkChunking() {
		EntityType type = DynamicJpaHelper.getType(getEMF(), "Employee");
		EntityManager em = getEMF().createEntityManager();

		Query query = em
				.createQuery("SELECT e.id FROM Employee e ORDER BY e.lastName ASC, e.firstName ASC");
		List<Long> empIds = query.getResultList();

		ReadAllQuery raq = new ReadAllQuery(type.getJavaClass());
		ExpressionBuilder eb = raq.getExpressionBuilder();
		raq.setSelectionCriteria(eb.get("id").in(empIds.subList(4, 8)));
		List<DynamicEntity> emps = JpaHelper.createQuery(raq, em)
				.getResultList();
		
		Assert.assertNotNull(emps);

		em.close();
	}
}
