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

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

import junit.framework.Assert;

import org.eclipse.persistence.extension.dynamic.*;
import org.junit.Test;

/**
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
@PersistenceContext(unitName = "employee")
public class ConfigTests extends EclipseLinkJPATest {

	@Test
	public void verifyMetadata() {
		Assert.assertNotNull(getEMF());

		EntityManager em = getEMF().createEntityManager();

		Assert.assertNotNull(em);

		// Ensure we are initialized and can talk to the database
		List results = em.createNativeQuery("SELECT e.* FROM DYNAMIC_EMP e")
				.getResultList();

		Assert.assertNotNull(results);
		Assert.assertTrue("Database table EMPLOYEE is empty",
				results.size() > 0);

		em.close();
	}

	@Test
	public void verifyEntityType() throws Exception {
		EntityType type = DynamicJpaHelper.getType(getEMF(), "Employee");
		Assert.assertNotNull(type);
		Assert.assertEquals("Employee", type.getName());
		Assert.assertEquals("model.Employee", type.getJavaClass().getName());
		Assert.assertEquals(DynamicEntity.class, type.getJavaClass()
				.getSuperclass());
	}

	@Test
	public void verifyEntityProperties() throws Exception {
		EntityType type = DynamicJpaHelper.getType(getEMF(), "Employee");
		Assert.assertNotNull(type);

		EntityProperty idProperty = type.getProperty("id");
		Assert.assertNotNull(idProperty);
		Assert.assertEquals("id", idProperty.getName());
		Assert.assertEquals(Integer.class, idProperty.getAttributeType());

		EntityProperty fnProperty = type.getProperty("firstName");
		Assert.assertNotNull(fnProperty);
		Assert.assertEquals("firstName", fnProperty.getName());
		Assert.assertEquals(String.class, fnProperty.getAttributeType());

		EntityProperty lnProperty = type.getProperty("lastName");
		Assert.assertNotNull(lnProperty);
		Assert.assertEquals("lastName", lnProperty.getName());
		Assert.assertEquals(String.class, lnProperty.getAttributeType());
	}

	@Test
	public void verifyNewInstance() throws Exception {
		EntityType type = DynamicJpaHelper.getType(getEMF(), "Employee");
		Assert.assertNotNull(type);

		DynamicEntity entity = type.newInstance();

		Assert.assertNotNull(entity);
		Assert.assertEquals("Employee", entity.getType().getName());
		Assert.assertEquals("model.Employee", entity.getClass().getName());
	}

}
