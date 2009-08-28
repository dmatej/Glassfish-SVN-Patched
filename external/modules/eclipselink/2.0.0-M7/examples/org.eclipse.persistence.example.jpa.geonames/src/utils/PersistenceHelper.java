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
 *     dclarke - initial GeoNames EclipseLink JPA example
 ******************************************************************************/
package utils;

import static org.eclipse.persistence.config.PersistenceUnitProperties.*;

import java.util.HashMap;
import java.util.Map;

import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.persistence.spi.PersistenceUnitTransactionType;

import org.eclipse.persistence.config.BatchWriting;
import org.eclipse.persistence.config.TargetServer;

/**
 * Utility class to facilitate the access to the EntityManagerFactory with
 * necessary property customizations for EclipseLink in JavaSE.
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
public class PersistenceHelper {

	public static final String PU_NAME = "geonames";

	public static EntityManagerFactory createEMF() {
		return createEMF(new HashMap(), true);
	}

	public static EntityManagerFactory createEMF(Map properties,
			boolean addSEProperties) {
		Map props = properties == null ? new HashMap() : properties;

		if (addSEProperties) {
			// Ensure RESOURCE_LOCAL transactions is used.
			props.put(TRANSACTION_TYPE,
					PersistenceUnitTransactionType.RESOURCE_LOCAL.name());

			// Configure the internal EclipseLink connection pool
			props.put(JDBC_DRIVER, "oracle.jdbc.OracleDriver");
			props.put(JDBC_URL, "jdbc:oracle:thin:@localhost:1521:ORCL");
			props.put(JDBC_USER, "scott");
			props.put(JDBC_PASSWORD, "tiger");
			props.put(JDBC_READ_CONNECTIONS_MIN, "1");
			props.put(JDBC_WRITE_CONNECTIONS_MIN, "1");

			props.put(BATCH_WRITING, BatchWriting.OracleJDBC);

			// Configure logging. FINE ensures all SQL is shown
			props.put(LOGGING_LEVEL, "FINE");
			props.put(LOGGING_TIMESTAMP, "false");
			props.put(LOGGING_THREAD, "false");
			props.put(LOGGING_SESSION, "false");

			// Ensure that no server-platform is configured
			props.put(TARGET_SERVER, TargetServer.None);
		}
		return Persistence.createEntityManagerFactory(PU_NAME, props);
	}
}
