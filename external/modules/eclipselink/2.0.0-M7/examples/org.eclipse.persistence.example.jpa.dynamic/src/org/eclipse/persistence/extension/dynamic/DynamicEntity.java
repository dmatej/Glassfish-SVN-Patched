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

import java.beans.PropertyChangeListener;
import java.io.StringWriter;

import org.eclipse.persistence.descriptors.changetracking.ChangeTracker;

/**
 * This abstract class is used to represent an entity which typically is not
 * realized in Java code. In combination with the DynamicClassLoader ASM is used
 * to generate subclasses that will work within EclipseLink's framework. Since
 * no concrete fields or methods exist on this class the mappings used must be
 * customized to use a custom AttributeAccessor ({@link EntityProperty.DynamicAttributeAccessor}).
 * <p>
 * <b>Type/Property Meta-model</b>: This dynamic entity approach also includes
 * a meta-model facade to simplify access to the types and property information
 * so that clients can more easily understand the model. Each {@link EntityType}
 * wraps the underlying EclipseLink relational-descriptor and the
 * {@link EntityProperty} wraps each mapping. The client application can use
 * these types and properties to facilitate generic access to the entity
 * instances and are required for creating new instances as well as for
 * accessing the Java class needed for JPA and EclipseLink native API calls.
 * 
 * @see DynamicEntity
 * @See DynamicAttributeAccessor
 * 
 * @author Doug Clarke
 * @since EclipseLink 1.0
 */
public abstract class DynamicEntity implements ChangeTracker {
	/**
	 * The persistent values indexed by the descriptor's mappings and the
	 * EntityType's corresponding property list.
	 */
	protected Object[] values;
	private EntityType type;
	/**
	 * ChangeListener used for attribute change tracking proceesed in the
	 * property
	 */
	private PropertyChangeListener changeListener = null;

	protected DynamicEntity(EntityType type) {
		this.type = type;
		this.values = new Object[type.getPropertiesSize()];
	}

	public EntityType getType() {
		return this.type;
	}

	public Object get(String propertyName) {
		return get(getType().getProperty(propertyName));
	}

	public Object get(int propertyIndex) {
		return get(getType().getProperty(propertyIndex));
	}

	public Object get(EntityProperty property) {
		return property.getFromEntity(this);
	}

	public DynamicEntity set(int propertyIndex, Object value) {
		return set(getType().getProperty(propertyIndex), value);
	}

	public DynamicEntity set(String propertyName, Object value) {
		return set(getType().getProperty(propertyName), value);
	}

	public DynamicEntity set(EntityProperty property, Object value) {
		return property.putInEntity(this, value);
	}

	private EntityCollectionProperty getCollectionProperty(String propertyName) {
		EntityProperty property = getType().getProperty(propertyName);
		if (!property.isCollection()) {
			throw DynamicEntityException.propertyNotCollection(property);
		}
		return (EntityCollectionProperty) property;
	}

	public Object addToCollection(String propertyName, Object value) {
		EntityCollectionProperty prop = getCollectionProperty(propertyName);
		return prop.addToCollection(this, value);
	}

	public Object removeFromCollection(String propertyName, Object value) {
		EntityCollectionProperty prop = getCollectionProperty(propertyName);
		return prop.removeFromCollection(this, value);
	}

	public Object getFromMap(String propertyName, Object key) {
		EntityCollectionProperty prop = getCollectionProperty(propertyName);
		return prop.getFromMap(this, key);
	}

	public Object putInMap(String propertyName, Object key, Object value) {
		EntityCollectionProperty prop = getCollectionProperty(propertyName);
		return prop.putInMap(this, key, value);
	}

	public Object removeKeyFromMap(String propertyName, Object key) {
		EntityCollectionProperty prop = getCollectionProperty(propertyName);
		return prop.removeKeyFromMap(this, key);
	}

	public PropertyChangeListener _persistence_getPropertyChangeListener() {
		return this.changeListener;
	}

	public void _persistence_setPropertyChangeListener(
			PropertyChangeListener listener) {
		this.changeListener = listener;
	}

	/**
	 * String representation of the dynamic entity using the entity type name
	 * and the primary key values.
	 */
	public String toString() {
		StringWriter writer = new StringWriter();

		writer.write(getType().getName());
		writer.write("(");

		for (EntityProperty prop : getType().getProperties()) {
			if (prop.isPrimaryKey()) {
				writer.write(prop.getName());
				writer.write("=" + prop.getRawFromEntity(this));
			}
		}

		writer.write(")");
		return writer.toString();
	}

}
