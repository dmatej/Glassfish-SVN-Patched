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
 *     dclarke - Example: Maintaining Collection Order (Bug 218321)
 *     			 http://wiki.eclipse.org/EclipseLink/Examples/JPA/Collectionordering
 *     
 *******************************************************************************/
package testing;

import static junit.framework.Assert.*;
import static org.eclipse.persistence.config.PersistenceUnitProperties.*;

import java.util.*;

import javax.persistence.*;

import model.*;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.jpa.JpaHelper;
import org.junit.Test;

public class CreateDatabase {

	/**
	 * @param args
	 */
	@Test
	public void createScheamandPopluate() {
		Map properties = new HashMap();
		properties.put(DDL_GENERATION, DROP_AND_CREATE);
		properties.put(DDL_GENERATION_MODE, DDL_DATABASE_GENERATION);

		EntityManagerFactory emf = null;
		EntityManager em = null;

		try {
			emf = Persistence.createEntityManagerFactory("persist-order", properties);
			em = emf.createEntityManager();
			createScheamandPopluate(em);

			assertOrderMatchesDatabase(emf, OrderLineItemTests.getOrderFromCache(em, "ORDER-1"));
		} finally {
			if (em != null && em.isOpen()) {
				if (em.getTransaction().isActive()) {
					em.getTransaction().rollback();
				}
				em.close();
			}
			if (emf != null && emf.isOpen()) {
				emf.close();
			}
		}
	}

	public static void createScheamandPopluate(EntityManager em) {
		em.getTransaction().begin();

		em.createQuery("DELETE FROM LineItem").executeUpdate();
		em.createQuery("DELETE FROM Order").executeUpdate();
		em.createQuery("DELETE FROM Product").executeUpdate();

		em.flush();
		em.clear();

		verifyCount(em, "LineItem", 0);
		verifyCount(em, "Order", 0);
		verifyCount(em, "Product", 0);

		Product prod1 = new Product(1, "PROD-1", null, 100d);
		Product prod2 = new Product(2, "PROD-2", null, 222d);
		Product prod3 = new Product(3, "PROD-3", null, 3.33d);
		Product prod4 = new Product(4, "PROD-4", null, 0.44d);

		em.persist(prod1);
		em.persist(prod2);
		em.persist(prod3);
		em.persist(prod4);

		Order order = new Order();
		order.setOrderNumber("ORDER-1");

		order.addLineItem(new LineItem(prod1, 1, prod1.getPrice()));
		order.addLineItem(new LineItem(prod2, 2, prod2.getPrice() * 0.9));
		order.addLineItem(new LineItem(prod3, 3, prod3.getPrice() * 0.5));

		em.persist(order);
		em.getTransaction().commit();

		verifyCount(em, "LineItem", 3);
		verifyCount(em, "Order", 1);
		verifyCount(em, "Product", 4);
	}

	private static void verifyCount(EntityManager em, String entityName, int count) {
		ClassDescriptor descriptor = JpaHelper.getEntityManager(em).getActiveSession().getClassDescriptorForAlias(entityName);

		Object countResult = em.createNativeQuery("SELECT COUNT(*) FROM " + descriptor.getTableName()).getSingleResult();
		int dbCount = -1;
		
		// The following checks are to handle types returned in various versions of EclipseLink
		if (countResult instanceof List) {
			countResult = ((List) countResult).get(0);
		}
		if (countResult instanceof Number) {
			dbCount = ((Number) countResult).intValue();
		} else {
			fail("Cound not convert count result to int: " + countResult);
		}
		
		assertEquals("Incorrect number of '" + entityName + "' in database", count, dbCount);
	}

	/**
	 * Verify the state of the provided Order matches the values in the
	 * database.
	 * 
	 * NOTE: A separate EntityManager is used from the one used in the test to
	 * ensure the state of the test is not effected (ie. Flushing)
	 */
	protected static void assertOrderMatchesDatabase(EntityManagerFactory emf, Order order) {
		EntityManager em = emf.createEntityManager();

		try {

			String dbDescription = (String) em.createQuery("SELECT o.description FROM Order o WHERE o.orderNum = :NUM").setParameter("NUM", order.getOrderNumber()).getSingleResult();

			assertEquals("Order.description does not match database", order.getDescription(), dbDescription);

			List<Object[]> dbLineItems = em.createQuery("SELECT li.lineNumber, li.productId, li.quantity, li.price FROM LineItem li WHERE li.orderNumber = :ORDER_NUM ORDER BY li.lineNumber")
					.setParameter("ORDER_NUM", order.getOrderNumber()).getResultList();

			assertEquals("Order.lineItems size does not match database", order.getLineItems().size(), dbLineItems.size());

			for (int index = 0; index < dbLineItems.size(); index++) {
				LineItem lineItem = order.getLineItems().get(index);
				Object[] dbValues = dbLineItems.get(index);

				assertEquals("LineItem[" + index + "].lineNumber does not match database", lineItem.getLineNumber(), ((Number) dbValues[0]).intValue());
				assertEquals("LineItem[" + index + "].productId does not match database", lineItem.getProductId(), ((Number) dbValues[1]).intValue());
				assertEquals("LineItem[" + index + "].quantity does not match database", lineItem.getQuantity(), ((Number) dbValues[2]).intValue());
				assertEquals("LineItem[" + index + "].price does not match database", lineItem.getPrice(), ((Number) dbValues[3]).doubleValue());
			}
		} finally {
			em.close();
		}

	}

	public static void main(String[] args) {
		new CreateDatabase().createScheamandPopluate();
	}
}
