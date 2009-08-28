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

import static org.eclipse.persistence.config.PersistenceUnitProperties.*;

import java.util.HashMap;
import java.util.Map;

import javax.persistence.*;

import org.eclipse.persistence.extension.dynamic.*;
import org.junit.*;

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
 * @since EclipseLink 1.0
 */
@PersistenceContext(unitName = "employee")
public abstract class EclipseLinkJPATest {

	/**
	 * Map of EMFs created during the running of the various test suites that
	 * extend this class. They are maintained in a static map and closed in the
	 * 
	 * @AfterClass method.
	 */
	private static Map<String, EntityManagerFactory> emfs = new HashMap<String, EntityManagerFactory>();

	/**
	 * This is he current EMF in use
	 */
	private EntityManagerFactory emf;

	protected EntityManagerFactory getEMF() {
		if (this.emf == null) {
			PersistenceContext context = getClass().getAnnotation(
					PersistenceContext.class);
			return getEMF(context.unitName());
		}
		return this.emf;
	}

	protected EntityManagerFactory getEMF(String unitName) {
		this.emf = emfs.get(unitName);

		if (this.emf == null) {
			this.emf = createEMF(unitName);
			emfs.put(unitName, this.emf);
		}

		return this.emf;
	}

	private EntityManagerFactory createEMF(String unitName) {
		try {
			Map properties = getEMFProperties();
			// Customize mappings for dynamic access
			properties.put(SESSION_CUSTOMIZER, "testing.DynamicSessionCustomizer");

			return Persistence.createEntityManagerFactory(unitName,
					properties);

		} catch (RuntimeException e) {
			System.out.println("Persistence.createEMF FAILED: "
					+ e.getMessage());
			e.printStackTrace();
			throw e;
		}
	}

	@After
	public void cleanupClosedEMF() {
		if (this.emf != null && !this.emf.isOpen()) {
			emfs.remove(emf);
			this.emf = null;
		}
	}

	@AfterClass
	public static void closeEMF() throws Exception {
		for (EntityManagerFactory emf : emfs.values()) {
			emf.close();
		}
		emfs.clear();
	}

	/**
	 * 
	 * @return
	 */
	protected static Map getEMFProperties() {
		Map properties = new HashMap();

		// Configure the internal EclipseLink connection pool
		properties.put(JDBC_DRIVER, "oracle.jdbc.OracleDriver");
		properties.put(JDBC_URL, "jdbc:oracle:thin:@localhost:1521:ORCL");
		properties.put(JDBC_USER, "scott");
		properties.put(JDBC_PASSWORD, "tiger");
		properties.put(JDBC_READ_CONNECTIONS_MIN, "1");
		properties.put(JDBC_WRITE_CONNECTIONS_MIN, "1");

		// Configure logging. FINE ensures all SQL is shown
		properties.put(LOGGING_LEVEL, "FINE");
		properties.put(LOGGING_TIMESTAMP, "false");
		properties.put(LOGGING_THREAD, "false");
		properties.put(LOGGING_SESSION, "false");

		return properties;
	}

	@Before
	public void populate() throws Exception {
		EntityManager em = getEMF().createEntityManager();

		EntityType empType = DynamicJpaHelper.getType(getEMF(), "Employee");

		Long count = (Long) em.createQuery("SELECT COUNT(e) FROM Employee e")
				.getSingleResult();

		if (count < 10) {
			em.getTransaction().begin();

			em.createNativeQuery("DELETE FROM DYNAMIC_EMP").executeUpdate();

			DynamicEntity emp = empType.newInstance();
			emp.set("id", 1);
			emp.set("firstName", "Bob");
			emp.set("lastName", "Smith");
			emp.set("salary", 10d);
			em.persist(emp);

			emp = empType.newInstance();
			emp.set("id", 2);
			emp.set("firstName", "Bob");
			emp.set("lastName", "Smith");
			emp.set("salary", 20d);
			em.persist(emp);

			emp = empType.newInstance();
			emp.set("id", 3);
			emp.set("firstName", "Bob");
			emp.set("lastName", "Smith");
			emp.set("salary", 30d);
			em.persist(emp);

			emp = empType.newInstance();
			emp.set("id", 4);
			emp.set("firstName", "Bob");
			emp.set("lastName", "Smith");
			emp.set("salary", 40d);
			em.persist(emp);

			emp = empType.newInstance();
			emp.set("id", 5);
			emp.set("firstName", "Bob");
			emp.set("lastName", "Smith");
			emp.set("salary", 50d);
			em.persist(emp);

			emp = empType.newInstance();
			emp.set("id", 6);
			emp.set("firstName", "Bob");
			emp.set("lastName", "Smith");
			emp.set("salary", 60d);
			em.persist(emp);

			emp = empType.newInstance();
			emp.set("id", 7);
			emp.set("firstName", "Bob");
			emp.set("lastName", "Smith");
			emp.set("salary", 70d);
			em.persist(emp);

			emp = empType.newInstance();
			emp.set("id", 8);
			emp.set("firstName", "Bob");
			emp.set("lastName", "Smith");
			emp.set("salary", 80d);
			em.persist(emp);

			emp = empType.newInstance();
			emp.set("id", 9);
			emp.set("firstName", "Bob");
			emp.set("lastName", "Smith");
			emp.set("salary", 90d);
			em.persist(emp);

			emp = empType.newInstance();
			emp.set("id", 10);
			emp.set("firstName", "Bob");
			emp.set("lastName", "Smith");
			emp.set("salary", 100d);
			em.persist(emp);
			em.getTransaction().commit();
		}
		em.close();
	}

}
