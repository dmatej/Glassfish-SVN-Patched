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

import org.eclipse.persistence.descriptors.RelationalDescriptor;
import org.eclipse.persistence.internal.jpa.CMP3Policy;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.mappings.DirectToFieldMapping;
import org.eclipse.persistence.sessions.server.Server;

/**
 * Utility class to simplify the dynamic definition of an EntityType during
 * session customization or at runtime.
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
public class EntityTypeFactory {

	public static EntityType create(String entityName,
			String qualifiedClassName, String tableName,
			DynamicClassLoader loader) {
		Class entityClass = null;
		try {
			entityClass = loader.loadClass(qualifiedClassName);
		} catch (ClassNotFoundException e) {
			throw new DynamicEntityException(
					"EntityTypeFactory.create could not find or create class for: "
							+ qualifiedClassName);
		}

		RelationalDescriptor descriptor = new RelationalDescriptor();
		descriptor.setTableName(tableName);
		descriptor.setJavaClass(entityClass);
		descriptor.setAlias(entityName);
		descriptor.setCMPPolicy(new CMP3Policy());

		return EntityType.getType(descriptor);
	}

	/**
	 * Add a DirectToFieldMapping to the descriptor within the provided
	 * EntityType and configure the attribute classification.
	 * 
	 * @param type
	 * @param name
	 * @param fieldName
	 * @param attributeType
	 * @param isIdentifier
	 *            Is primary key column
	 * @return The property created for the new mapping
	 */
	public static EntityProperty addBasicProperty(EntityType type, String name,
			String fieldName, Class attributeType, boolean isIdentifier) {
		DirectToFieldMapping mapping = (DirectToFieldMapping) type
				.getDescriptor().addDirectMapping(name, fieldName);
		EntityProperty property = type.addProperty(mapping, attributeType);

		if (isIdentifier) {
			type.getDescriptor().addPrimaryKeyFieldName(fieldName);
			((CMP3Policy) type.getDescriptor().getCMPPolicy())
					.setPKClass(attributeType);
		}

		return property;
	}

	/**
	 * Add the newly defined EntityType and its underlying (uninitialized)
	 * descriptor to the EntityManagerFactory's session.
	 * 
	 * @see EntityTypeFactory#addToSession(Server, EntityType)
	 */
	public static void addToPersistenceUnit(EntityManagerFactory emf,
			EntityType type) {
		addToSession(JpaHelper.getServerSession(emf), type);
	}

	/**
	 * Add the newly defined EntityType and its underlying (uninitialized)
	 * descriptor to the provided session session. This adding to the session
	 * will cause the descriptor to be initialized if the session is already
	 * logged in. Otherwise it will just be added and initialization will happen
	 * later during login.
	 */
	public static void addToSession(Server session, EntityType type) {
		session.addDescriptor(type.getDescriptor());
	}
}
