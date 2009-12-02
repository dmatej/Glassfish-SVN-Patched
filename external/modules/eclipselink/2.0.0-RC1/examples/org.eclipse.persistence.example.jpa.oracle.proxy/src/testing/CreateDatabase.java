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

import java.util.Map;

import javax.persistence.*;

/**
 * Utility class to create the database schema and populate it for the Employee
 * JPA example using XML configuration. This
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
public class CreateDatabase {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Map properties = EMFHelper.javaSEProperties();

		// Add in properties to have the database re-created
		properties.put(DDL_GENERATION, DROP_AND_CREATE);
		properties.put(LOGGING_LEVEL, "ALL");

		EntityManagerFactory emf = Persistence.createEntityManagerFactory(
				"proxy-employee", properties);
		EntityManager em = emf.createEntityManager();

		em.close();
		emf.close();
	}
}
