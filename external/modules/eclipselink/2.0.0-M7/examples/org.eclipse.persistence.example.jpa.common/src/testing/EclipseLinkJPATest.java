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
package testing;

import java.util.HashMap;
import java.util.Map;

import javax.persistence.*;

import static org.junit.Assert.*;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.jpa.JpaHelper;
import org.junit.After;
import org.junit.AfterClass;

import testing.util.QuerySQLTracker;

/**
 * Base test case for testing a JPA persistence unit in JavaSE using JUnit4.
 * 
 * Through the usage
 * 
 * @PersistenceContext on subclasses a developer can indicate the persistence
 *                     unit name that the
 * @BeforeClass method should use to access the entityManager.
 * 
 * @author dclarke
 * @since EclipseLink 1.1
 */
public abstract class EclipseLinkJPATest {

	/**
	 * This is he current EMF in use
	 */
	private static EntityManagerFactory emf;

	private EntityManager entityManager;

	protected EntityManagerFactory getEMF() {
		if (emf == null) {
			emf = createEMF(getUnitName());
		}

		return emf;
	}

	protected EntityManager getEntityManager() {
		if (this.entityManager == null) {
			this.entityManager = getEMF().createEntityManager();
		}

		return this.entityManager;
	}

	protected EntityManagerFactory createEMF(String unitName) {
		if (emf != null) {
			if (emf.isOpen()) {
				emf.close();
			}
		}

		assertNotNull("EclipseLinkJPATest.createEMF:: Null unit name", unitName);

		try {
			return createEMF(unitName, null);
		} catch (RuntimeException e) {
			System.out.println("Persistence.createEMF FAILED: " + e.getMessage());
			e.printStackTrace();
			throw e;
		}
	}

	private String getUnitName() {
		PersistenceContext context = null;
		Class javaClass = getClass();

		while (context == null && javaClass != Object.class) {
			context = (PersistenceContext) javaClass.getAnnotation(PersistenceContext.class);
			javaClass = javaClass.getSuperclass();
		}
		assertNotNull("No @PersistenceContext found", context);

		return context.unitName();
	}

	/**
	 * 
	 * @param properties
	 * @return
	 */
	protected EntityManagerFactory createEMF(String unitName, Map properties) {
		Map emfProps = getEMFProperties();

		if (properties != null) {
			emfProps.putAll(properties);
		}

		try {
			EntityManagerFactory emf = Persistence.createEntityManagerFactory(unitName, emfProps);
			QuerySQLTracker.install(JpaHelper.getServerSession(emf));
			return emf;
		} catch (RuntimeException e) {
			System.out.println("Persistence.createEMF FAILED: " + e.getMessage());
			e.printStackTrace();
			throw e;
		}
	}

	/**
	 * 
	 * @return
	 */
	protected Map getEMFProperties() {
		Map properties = new HashMap();

		// Use the system properties to override the ones specified in the
		// persistence.xml
		// This allows the caller< Example build/test system, to override the
		// default values
		properties.putAll(System.getProperties());

		return properties;
	}

	/**
	 * Lookup a descriptor in the current EMF's server session. Asserts that the
	 * descriptor is not null
	 */
	protected ClassDescriptor getDescriptor(String entityName) {
		ClassDescriptor descriptor = JpaHelper.getServerSession(getEMF()).getClassDescriptorForAlias(entityName);
		assertNotNull("No descriptor found for entityName (alias): " + entityName, descriptor);
		return descriptor;
	}

	/**
	 * 
	 */
	protected QuerySQLTracker getQuerySQLTracker(EntityManager em) {
		return QuerySQLTracker.getTracker(JpaHelper.getEntityManager(em).getActiveSession());
	}

	@After
	public void cleanupClosedEMF() {
		if (this.entityManager != null) {

			if (this.entityManager.getTransaction().isActive()) {
				this.entityManager.getTransaction().rollback();
			}
			if (this.entityManager.isOpen()) {
				this.entityManager.close();
			}
		}
		this.entityManager = null;

		if (emf != null) {
			if (!emf.isOpen()) {
				emf = null;
			} else {
				QuerySQLTracker.getTracker(JpaHelper.getServerSession(emf)).reset();
			}
		}
	}

	@AfterClass
	public static void closeEMF() throws Exception {
		if (emf != null && emf.isOpen()) {
			emf.close();
			emf = null;
		}
	}

}
