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
 * 				- ported from earlier Oracle Toplink examples
 ******************************************************************************/
package testing;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.persistence.PersistenceContext;

import junit.framework.Assert;

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
 * @since EclipseLink 1.1.2
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

		Assert.assertNotNull("EclipseLinkJPATest.createEMF:: Null unit name",
				unitName);

		try {
			return createEMF(unitName, null);
		} catch (RuntimeException e) {
			System.out.println("Persistence.createEMF FAILED: "
					+ e.getMessage());
			e.printStackTrace();
			throw e;
		}
	}

	protected String getUnitName() {
		PersistenceContext context = null;
		Class javaClass = getClass();

		while (context == null && javaClass != Object.class) {
			context = (PersistenceContext) javaClass
					.getAnnotation(PersistenceContext.class);
			javaClass = javaClass.getSuperclass();
		}
		Assert.assertNotNull("No @PersistenceContext found", context);

		return context.unitName();
	}

	/**
	 * 
	 * @param properties
	 * @return
	 * @throws Exception
	 */
	protected EntityManagerFactory createEMF(String unitName, Map properties) {
		try {
			Map emfProps = getEMFProperties();

			if (properties != null) {
				emfProps.putAll(properties);
			}

			EntityManagerFactory emf = Persistence.createEntityManagerFactory(
					unitName, emfProps);
			QuerySQLTracker.install(JpaHelper.getServerSession(emf));
			return emf;
		} catch (Exception e) {
			System.out.println("Persistence.createEMF FAILED: "
					+ e.getMessage());
			e.printStackTrace();
			throw new RuntimeException("EclipseLinkJPATest.createEMF("
					+ unitName + ", properties) - failed", e);
		}
	}

	/**
	 * 
	 * @return
	 */
	protected Map getEMFProperties() {
		Map properties = new HashMap();

		try {
			File examplePropertiesFile = new File(
					"eclipselink-examples.properties");
			if (examplePropertiesFile.exists()) {
				Properties exampleProps = new Properties();
				InputStream in = new FileInputStream(examplePropertiesFile);
				exampleProps.load(in);
				in.close();
				properties.putAll(exampleProps);
			}
		} catch (Exception e) {
			// TODO
		}

		properties.putAll(System.getProperties());

		return properties;
	}

	protected QuerySQLTracker getQuerySQLTracker(EntityManager em) {
		return QuerySQLTracker.getTracker(JpaHelper.getEntityManager(em)
				.getActiveSession());
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
				QuerySQLTracker.getTracker(JpaHelper.getServerSession(emf))
						.reset();
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
