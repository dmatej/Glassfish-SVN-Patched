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

import java.net.URL;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;

import org.eclipse.persistence.extension.dynamic.*;
import org.eclipse.persistence.internal.jpa.EntityManagerFactoryImpl;
import org.eclipse.persistence.internal.jpa.EntityManagerSetupImpl;
import org.eclipse.persistence.internal.jpa.deployment.SEPersistenceUnitInfo;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.tools.schemaframework.SchemaManager;
import org.junit.Assert;
import org.junit.Test;

/**
 * These tests show the usage Dynamic EclipseLink JPA without requiring a
 * persistence.xml file.
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
public class PUWithoutXML {

	@Test
	public void createSimpleDynamicEntity() throws Exception {

		// Create a dynamic EMF without requiring persistence.xml
		DynamicClassLoader loader = new DynamicClassLoader(null,
				DynamicEntity.class);
		SEPersistenceUnitInfo puInfo = new SEPersistenceUnitInfo();
		puInfo.setClassLoader(loader);
		puInfo.setPersistenceUnitName("dynamic");
		// This root URL config should go away when bug 225321 is fixed
		puInfo
				.setPersistenceUnitRootUrl(new URL(
						"file:/C:/Eclipse/EclipseLink/trunk/examples/org.eclipse.persistence.example.jpa.dynamic/classes/"));

		EntityManagerSetupImpl setup = new EntityManagerSetupImpl();
		setup.predeploy(puInfo, null);
		// This replaces Persistence.createEntityManagerFactory
		EntityManagerFactory emf = new EntityManagerFactoryImpl(setup,
				EclipseLinkJPATest.getEMFProperties());

		// Create a new entity type on the fly
EntityType employeeType = EntityTypeFactory.create("Employee",
		"DynamicEntity$Employee", "DYNAMIC_EMP", loader);
EntityTypeFactory.addBasicProperty(employeeType, "id", "EMP_ID",
		Integer.class, true);
EntityTypeFactory.addBasicProperty(employeeType, "firstName", "F_NAME",
		String.class, false);
EntityTypeFactory.addBasicProperty(employeeType, "lastName", "L_NAME",
		String.class, false);
EntityTypeFactory.addBasicProperty(employeeType, "salary", "SALARY",
		double.class, false);
EntityTypeFactory.addToPersistenceUnit(emf, employeeType);

		Assert.assertEquals(Integer.class, employeeType.getDescriptor()
				.getCMPPolicy().getPKClass());

		// Drop and create the table
		new SchemaManager(JpaHelper.getServerSession(emf))
				.replaceDefaultTables();

		// Test basic CRUD using standard JPA
		EntityManager em = emf.createEntityManager();

		DynamicEntity emp1 = (DynamicEntity) em.find(employeeType
				.getJavaClass(), 1);
		Assert.assertNull(emp1);

		em.getTransaction().begin();

		emp1 = employeeType.newInstance();
		emp1.set("id", 1);
		emp1.set("firstName", "John");
		emp1.set("lastName", "Doe");

		em.persist(emp1);
		em.getTransaction().commit();

		em.clear();

		emp1 = (DynamicEntity) em.find(employeeType.getJavaClass(), 1);
		Assert.assertNotNull(emp1);

		emp1 = (DynamicEntity) em
				.createQuery(
						"SELECT e FROM Employee e WHERE e.firstName = :FNAME AND e.lastName = :LNAME")
				.setParameter("FNAME", "John").setParameter("LNAME", "Doe")
				.getSingleResult();

		Assert.assertNotNull(emp1);
		Assert.assertEquals(1, emp1.get("id"));

		emf.close();
	}

}
