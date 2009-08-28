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
package org.eclipse.persistence.extension.dynamic;

import javax.persistence.EntityManagerFactory;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.jpa.JpaHelper;

/**
 * A DynamicSessionFactory is the factory to bootstrap a session that will use
 * dynamic entity types as well as being a helper class for using these types.
 * The standard TopLink interface has many dependencies on the concrete class.
 * These concrete classes will not be available for the application developer
 * and instead they must use aliases or assumed String class names. This factory
 * will assist in the writing of TopLink enabled applications.
 * 
 * @author Doug Clarke
 * @since EclipseLink 1.0
 */
public class DynamicJpaHelper {

	/**
	 * Lookup the dynamic type for an alias. This is required to get the type
	 * for factory creation but can also be used to provide the application with
	 * access to the metamodel (type and properties) allowing for dynamic use as
	 * well as optimized data value retrieval from an entity.
	 * 
	 * @param typeName
	 * @return
	 */
	public static EntityType getType(EntityManagerFactory emf, String typeName) {
		EntityType type = null;
		try {
			ClassDescriptor cd = JpaHelper.getServerSession(emf)
					.getClassDescriptorForAlias(typeName);
			type = EntityType.getType(cd);
		} catch (NullPointerException e) { // Workaround for bug ???
			throw DynamicEntityException.invalidTypeName(typeName);
		}
		if (type == null) {
			throw DynamicEntityException.invalidTypeName(typeName);
		}

		return type;
	}

}
