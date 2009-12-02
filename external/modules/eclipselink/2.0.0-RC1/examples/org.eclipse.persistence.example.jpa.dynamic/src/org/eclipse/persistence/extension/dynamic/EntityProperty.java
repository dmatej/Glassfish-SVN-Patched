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

import java.beans.PropertyChangeEvent;

import org.eclipse.persistence.exceptions.DescriptorException;
import org.eclipse.persistence.indirection.IndirectContainer;
import org.eclipse.persistence.indirection.ValueHolderInterface;
import org.eclipse.persistence.internal.descriptors.InstanceVariableAttributeAccessor;
import org.eclipse.persistence.internal.helper.ClassConstants;
import org.eclipse.persistence.internal.helper.Helper;
import org.eclipse.persistence.internal.security.PrivilegedAccessHelper;
import org.eclipse.persistence.mappings.*;
import org.eclipse.persistence.mappings.foundation.AbstractDirectMapping;

/**
 * <b>Purpose</b>: Metadata model for persistent property of a dynamic entity.
 * <p>
 * The property is used within the EntityType metadata model to represent a
 * persistent property wrapping the underlying TopLink mapping. It can be used
 * by an application to access the structure of the dynamic entity model as well
 * as provide more optimal access to data values within an entity.
 * <p>
 * 
 * @author Doug Clarke
 * @since EclipseLink 1.0
 */
public class EntityProperty {
	private EntityType type;
	private DatabaseMapping mapping;
	private int index;
	protected boolean primaryKey = false;

	protected EntityProperty(EntityType type, DatabaseMapping mapping, int index) {
		this.type = type;
		this.mapping = mapping;
		this.index = index;

		if (type.getDescriptor().getPrimaryKeyFields().contains(
				mapping.getField())) {
			this.primaryKey = true;
		}
		AttributeAccessor accessor = new DynamicAttributeAccessor(this);
		mapping.setAttributeAccessor(accessor);
	}

	public EntityType getType() {
		return this.type;
	}

	public String getName() {
		return getMapping().getAttributeName();
	}

	public Class getAttributeType() {
		return getMapping().getAttributeClassification();
	}

	public void setAttributeType(Class javaClass) {
		((DynamicAttributeAccessor) getMapping().getAttributeAccessor())
				.setAttributeClass(javaClass);
	}

	protected EntityType getEntityType() {
		return this.type;
	}

	protected DatabaseMapping getMapping() {
		return this.mapping;
	}

	public int getIndex() {
		return this.index;
	}

	public boolean isPrimaryKey() {
		return this.primaryKey;
	}

	public boolean isCollection() {
		return false;
	}

	protected void putRawInEntity(DynamicEntity entity, Object value) {
		getMapping().setAttributeValueInObject(entity, value);
	}

	protected DynamicEntity putInEntity(DynamicEntity entity, Object value) {
		Object currentValue = getRawFromEntity(entity);

		if (currentValue instanceof IndirectContainer) {
			currentValue = ((IndirectContainer) currentValue).getValueHolder();
		}

		if (entity._persistence_getPropertyChangeListener() != null) {
			PropertyChangeEvent event = new PropertyChangeEvent(entity,
					getName(), currentValue, value);
			entity._persistence_getPropertyChangeListener().propertyChange(
					event);
		}

		if (currentValue instanceof ValueHolderInterface) {
			((ValueHolderInterface) currentValue).setValue(value);
		} else {
			putRawInEntity(entity, value);
		}
		return entity;
	}

	protected Object getRawFromEntity(DynamicEntity entity) {
		return getMapping().getAttributeValueFromObject(entity);
	}

	protected Object getFromEntity(DynamicEntity entity) {
		Object currentValue = getRawFromEntity(entity);

		if (currentValue == NULL_ENTRY) {
			return null;
		}
		if (currentValue instanceof ValueHolderInterface) {
			return ((ValueHolderInterface) currentValue).getValue();
		}
		if (currentValue instanceof IndirectContainer) {
			return ((IndirectContainer) currentValue).getValueHolder()
					.getValue();
		}
		return currentValue;

	}

	/**
	 * Allows property assign value for new instances. For DirectMappings this
	 * does nothing. Provided to allow subclasses to initialize relationships.
	 */
	protected void initializeValue(DynamicEntity entity) {
		if (getAttributeType().isPrimitive()) {
			Object value = null;

			if (getAttributeType() == ClassConstants.PBOOLEAN) {
				value = true;
			} else if (getAttributeType() == ClassConstants.PINT) {
				value = 0;
			} else if (getAttributeType() == ClassConstants.PSHORT) {
				value = 0;
			} else if (getAttributeType() == ClassConstants.PLONG) {
				value = 0l;
			} else if (getAttributeType() == ClassConstants.PDOUBLE) {
				value = 0d;
			} else if (getAttributeType() == ClassConstants.PFLOAT) {
				value = 0f;
			}

			if (value == null) {
				throw new DynamicEntityException(
						"EntityProperty could not initialize default value for primitive: "
								+ getAttributeType());
			}
			putInEntity(entity, value);
		}
	}

	/**
	 * Returns true if property requires initialization on new instance
	 * creation. Allows for indirect relationships to create the necessary
	 * value-holder
	 */
	protected boolean requiresInitialization() {
		return getAttributeType().isPrimitive();
	}

	public String toString() {
		return Helper.getShortClassName(this) + "[" + getType().getName() + "."
				+ getName() + "] -> " + getMapping();
	}

	protected static final NullEntry NULL_ENTRY = new NullEntry();

	protected static class NullEntry {
	}

	public class DynamicAttributeAccessor extends
			InstanceVariableAttributeAccessor {
		private EntityProperty property;
		private Class attributeClass = ClassConstants.OBJECT;

		private DynamicAttributeAccessor(EntityProperty property) {
			super();
			this.property = property;

			if (property.getMapping().isForeignReferenceMapping()) {
				ForeignReferenceMapping frm = (ForeignReferenceMapping) property
						.getMapping();

				if (frm.getIndirectionPolicy().usesIndirection()
						&& !frm.getIndirectionPolicy()
								.usesTransparentIndirection()) {
					this.attributeClass = ValueHolderInterface.class;
				}
			}
		}

		public void initializeAttributes(Class theJavaClass)
				throws DescriptorException {
			try {
				setAttributeField(PrivilegedAccessHelper.getField(theJavaClass,
						"values", true));
			} catch (NoSuchFieldException exception) {
				throw DescriptorException
						.noSuchFieldWhileInitializingAttributesInInstanceVariableAccessor(
								getAttributeName(), theJavaClass.getName(),
								exception);
			} catch (SecurityException exception) {
				throw DescriptorException
						.securityWhileInitializingAttributesInInstanceVariableAccessor(
								getAttributeName(), theJavaClass.getName(),
								exception);
			}
		}

		public void setAttributeClass(Class javaClass) {
			this.attributeClass = javaClass;

			if (getMapping().isAbstractDirectMapping()) {
				((AbstractDirectMapping) getMapping())
						.setAttributeClassification(javaClass);
			}
		}

		public Object getAttributeValueFromObject(Object anObject)
				throws DescriptorException {
			Object[] values = (Object[]) super
					.getAttributeValueFromObject(anObject);
			Object value = values[getProperty().getIndex()];

			if (value == NULL_ENTRY) {
				value = null;
			}
			return value;
		}

		public void setAttributeValueInObject(Object anObject, Object value)
				throws DescriptorException {
			if (value != null && attributeClass == ClassConstants.OBJECT) {
				this.attributeClass = value.getClass();
			}

			Object[] values = (Object[]) super
					.getAttributeValueFromObject(anObject);
			values[getProperty().getIndex()] = value == null ? NULL_ENTRY
					: value;
		}

		public Class getAttributeType() {
			return this.attributeClass;
		}

		public EntityProperty getProperty() {
			return this.property;
		}
	}
}
