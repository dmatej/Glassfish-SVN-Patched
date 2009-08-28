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

import static org.junit.Assert.assertEquals;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

import org.eclipse.persistence.config.PessimisticLock;
import org.eclipse.persistence.config.QueryHints;
import org.eclipse.persistence.extension.dynamic.DynamicEntity;
import org.eclipse.persistence.extension.dynamic.DynamicJpaHelper;
import org.eclipse.persistence.extension.dynamic.EntityType;
import org.junit.After;
import org.junit.Test;

@PersistenceContext(unitName = "employee")
public class TransactionExamples extends EclipseLinkJPATest {

	@Test
	public void pessimisticLocking() throws Exception {
		EntityManager em = getEMF().createEntityManager();

		// Find the Employee with the minimum ID
		Object minId = em.createQuery("SELECT MIN(e.id) from Employee e")
				.getSingleResult();

		em.getTransaction().begin();

		// Lock Employee using query with hint
		DynamicEntity emp = (DynamicEntity) em.createQuery(
				"SELECT e FROM Employee e WHERE e.id = :ID").setParameter("ID",
				minId).setHint(QueryHints.PESSIMISTIC_LOCK,
				PessimisticLock.Lock).getSingleResult();

		emp.set("firstName", emp.get("firstName") + "+");

		em.getTransaction().commit();
	}

	@Test
	public void createNewObject() throws Exception {
		EntityType type = DynamicJpaHelper.getType(getEMF(), "Employee");
		EntityManager em = getEMF().createEntityManager();

		try {
			em.getTransaction().begin();

			DynamicEntity newEmp = (DynamicEntity) type.newInstance();
			newEmp.set("id", 111222333);
			newEmp.set("firstName", "Doug");
			newEmp.set("lastName", "Clarke");

			assertEquals(0.0d, newEmp.get("salary"));

			em.getTransaction().commit();
		} finally {
			em.close();
		}
	}

	/**
	 * 
	 * @throws Exception
	 */
	@After
	public void cleanUpData() throws Exception {
		EntityManager em = getEMF().createEntityManager();

		try {
			em.getTransaction().begin();
			// em.remove(em.find(Employee.class, newEmp.getId()));
			em.getTransaction().commit();
		} finally {
			if (em.getTransaction().isActive()) {
				em.getTransaction().rollback();
			}

			em.close();
		}
	}
}
