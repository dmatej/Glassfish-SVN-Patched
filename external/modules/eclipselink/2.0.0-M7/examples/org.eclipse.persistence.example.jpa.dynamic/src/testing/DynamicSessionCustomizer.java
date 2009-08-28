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
 *     dclarke - Written as part of Dynamic JPA Example
 ******************************************************************************/
package testing;

import org.eclipse.persistence.config.SessionCustomizer;
import org.eclipse.persistence.extension.dynamic.DynamicClassLoader;
import org.eclipse.persistence.extension.dynamic.DynamicEntity;
import org.eclipse.persistence.extension.dynamic.EntityType;
import org.eclipse.persistence.extension.dynamic.EntityTypeFactory;
import org.eclipse.persistence.sessions.Session;
import org.eclipse.persistence.sessions.server.Server;

/**
 * 
 * 
 * @author Doug Clarke
 * @since EclipseLink 1.0
 */
public class DynamicSessionCustomizer implements SessionCustomizer {
	public void customize(Session session) {
		DynamicClassLoader loader = new DynamicClassLoader(null,
				DynamicEntity.class);
		session.getPlatform().getConversionManager().setLoader(loader);

		EntityType employeeType = EntityTypeFactory.create("Employee", "model.Employee", "DYNAMIC_EMP", loader);
		EntityTypeFactory.addBasicProperty(employeeType, "id", "EMP_ID", Integer.class, true);
		EntityTypeFactory.addBasicProperty(employeeType, "firstName", "F_NAME", String.class, false);
		EntityTypeFactory.addBasicProperty(employeeType, "lastName", "L_NAME", String.class, false);
		EntityTypeFactory.addBasicProperty(employeeType, "salary", "SALARY", double.class, false);

		EntityTypeFactory.addToSession((Server) session, employeeType);
	}
}
