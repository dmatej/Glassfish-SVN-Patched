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

import java.util.*;

import org.eclipse.persistence.internal.helper.DatabaseField;
import org.eclipse.persistence.mappings.*;

/**
 * 
 * @author Doug Clarke
 * @since EclipseLink 1.0
 */
public class EntityReferenceProperty extends EntityProperty {
	protected EntityReferenceProperty(EntityType type,
			ForeignReferenceMapping mapping, int index) {
		super(type, mapping, index);

		if (getMapping().isOneToOneMapping()) {
			Vector fkFields = getOneToOneMapping().getForeignKeyFields();
			for (Iterator i = fkFields.iterator(); i.hasNext();) {
				DatabaseField fkField = (DatabaseField) i.next();
				if (getType().getDescriptor().getPrimaryKeyFields().contains(
						fkField)) {
					this.primaryKey = true;
				}
			}
		}
	}

	private OneToOneMapping getOneToOneMapping() {
		return (OneToOneMapping) super.getMapping();
	}

	public boolean isMap() {
		return getMapping().getContainerPolicy().isMapPolicy();
	}

	protected CollectionMapping getCollectionMapping() {
		return (CollectionMapping) getMapping();
	}

	private Collection getCollection(DynamicEntity entity) {
		if (isMap()) {
			throw new DynamicEntityException(
					"Cannot retieve Map from collection mapping: "
							+ getType().getName() + "." + getName());
		}

		Object currentValue = super.getFromEntity(entity);

		if (currentValue == null) {
			currentValue = getMapping().getContainerPolicy()
					.containerInstance();
			putRawInEntity(entity, currentValue);
		}

		return (Collection) currentValue;
	}

	public Object addToCollection(DynamicEntity entity, Object value) {
		return getCollection(entity).add(value);
	}

	public Object removeFromCollection(DynamicEntity entity, Object value) {
		return getCollection(entity).remove(value);
	}

	private Map getMap(DynamicEntity entity) {
		if (!isMap()) {
			throw new DynamicEntityException(
					"Cannot retieve non-Map from map mapping: "
							+ getType().getName() + "." + getName());
		}

		Object currentValue = super.getFromEntity(entity);

		if (currentValue == null) {
			currentValue = getMapping().getContainerPolicy()
					.containerInstance();
			putRawInEntity(entity, currentValue);
		}

		return (Map) currentValue;
	}

	public Object getFromMap(DynamicEntity entity, Object key) {
		return getMap(entity).get(key);
	}

	public Object putInMap(DynamicEntity entity, Object key, Object value) {
		return getMap(entity).put(key, value);
	}

	public Object removeKeyFromMap(DynamicEntity entity, Object key) {
		return getMap(entity).remove(key);
	}

	protected boolean requiresInitialization() {
		return ((ForeignReferenceMapping) getMapping()).usesIndirection();
	}

	protected void initializeValue(DynamicEntity entity) {

	}
}
