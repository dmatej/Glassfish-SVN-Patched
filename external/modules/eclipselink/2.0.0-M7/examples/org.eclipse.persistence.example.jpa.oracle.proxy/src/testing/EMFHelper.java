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
 *     dclarke - Initial demo contribution
 ******************************************************************************/
package testing;

import static org.eclipse.persistence.config.PersistenceUnitProperties.*;

import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.sql.DataSource;

import oracle.jdbc.pool.OracleDataSource;

import org.junit.AfterClass;
import org.junit.BeforeClass;

/**
 * Base test case for common example JPA access.
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
public abstract class EMFHelper {
	private static EntityManagerFactory emf;

	protected static EntityManagerFactory getEMF() {
		return emf;
	}

	protected static void setEMF(EntityManagerFactory emfValue) {
		emf = emfValue;
	}

	@BeforeClass
	public static void createEMF() throws Exception {
		try {
			emf = Persistence.createEntityManagerFactory("proxy-employee",
					javaSEProperties());
		} catch (RuntimeException e) {
			System.out.println("Persistence.createEMF FAILED: "
					+ e.getMessage());
			e.printStackTrace();
			throw e;
		}
	}

	@AfterClass
	public static void closeEMF() throws Exception {
		emf.close();
	}

	/**
	 * @param args
	 */
	public static Map javaSEProperties() {
		Map properties = new HashMap();

		properties.put(NON_JTA_DATASOURCE, createDataSource());
		properties.put(LOGGING_LEVEL, "FINE");

		return properties;
	}
	
	private static DataSource createDataSource() {
		OracleDataSource ods;
		try {
			ods = new OracleDataSource();
		} catch (SQLException ex) {
			throw new RuntimeException("Failed creating OracleDataSource", ex);
		}
		ods.setURL("jdbc:oracle:thin:@localhost:1521:ORCL");
		ods.setUser("scott");
		ods.setPassword("tiger");

		return ods;
	}
}
