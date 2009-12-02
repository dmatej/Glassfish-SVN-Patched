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

import org.eclipse.persistence.exceptions.EclipseLinkException;

/**
 * Custom exception type that provides information about failure cases
 * encountered when using a GenericEntity with TopLink. Any failures that are
 * not specific to GenericEntity use will still involve the standard TopLink
 * exceptions.
 * 
 * @author Doug Clarke
 * @since EclipseLink 1.0
 */
public class DynamicEntityException extends EclipseLinkException {
	public static final String ILLEGAL_MODIFY_SHARED = "Illegal attempt to modify shared cache instance on: ";

	public DynamicEntityException(String message) {
		super(message);
	}

	public static DynamicEntityException invalidTypeName(String typeName) {
		return new DynamicEntityException("Invalid DynamicEntity type name: "
				+ typeName);
	}

	public static DynamicEntityException invalidPropertyName(EntityType type,
			String propertyName) {
		return new DynamicEntityException("Invalid DynamicEntity[" + type
				+ "] property name: " + propertyName);
	}

	public static DynamicEntityException invalidPropertyIndex(EntityType type,
			int propertyIndex) {
		return new DynamicEntityException("Invalid DynamicEntity[" + type
				+ "] property index: " + propertyIndex);
	}

	public static DynamicEntityException illegalMergeOfManagedInstance(
			DynamicEntity entity) {
		return new DynamicEntityException("Illegal Merge attempt of: " + entity
				+ " of type: " + entity.getClass());
	}

	public static DynamicEntityException featureNotSupported(String message) {
		return new DynamicEntityException(
				"DynamicEntity does not yet support: " + message);
	}

	public static DynamicEntityException propertyNotCollection(
			EntityProperty property) {
		return new DynamicEntityException(
				"DynamicEntity:: Cannot return collection for: " + property);
	}
}
