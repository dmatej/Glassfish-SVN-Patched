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

import java.util.Collection;
import java.util.Map;

import org.eclipse.persistence.mappings.CollectionMapping;

/**
 * Property used to represent an attribute mapped by either a 1:M or M:M mapping
 * with either a Map or Collection (Set/List) container type
 * 
 * @author Doug Clarke
 * @since EclipseLink 1.0
 */
public class EntityCollectionProperty extends EntityReferenceProperty {
	protected EntityCollectionProperty(EntityType type,
			CollectionMapping mapping, int index) {
		super(type, mapping, index);
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

	public boolean isCollection() {
		return getMapping().getContainerPolicy().isCollectionPolicy();
	}

	protected Object initialValue() {
		return getCollectionMapping().getContainerPolicy().containerInstance();
	}
}
