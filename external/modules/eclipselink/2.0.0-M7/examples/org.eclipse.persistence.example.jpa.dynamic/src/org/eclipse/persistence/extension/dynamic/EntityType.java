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

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.*;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.descriptors.changetracking.AttributeChangeTrackingPolicy;
import org.eclipse.persistence.exceptions.DescriptorException;
import org.eclipse.persistence.internal.security.PrivilegedAccessHelper;
import org.eclipse.persistence.mappings.*;

/**
 * An EntityType provides a metadata facade into the EclipseLink
 * object-relational metadata (descriptors & mappings) with specific knowledge
 * of the entity types being dynamic.
 * 
 * @author Doug Clarke
 * @since EclipseLink 1.0
 */
public class EntityType {
	/**
	 * Property name used to store the singleton EntityType on each descriptor.
	 */
	private static final String DESCRIPTOR_PROPERTY = "ENTITY_TYPE";
	/**
	 * Method name on EntityType used to by the descriptor's instantiation
	 * policy. The EntityType instance functions as the factory object in the
	 * policy.
	 */
	private static String FACTORY_METHOD = "newInstance";

	private ClassDescriptor descriptor;
	/**
	 * Map of properties keyed by name.
	 */
	private Map<String, EntityProperty> propertiesMap;
	/**
	 * List of properties. This is the same properties as the propertiesMap but
	 * is used to match the order of the mappings and aligns with the values[]
	 * in the DynamicEntity instances. This allows the values to be accessed by
	 * index as well as by property name/instance.
	 */
	private ArrayList<EntityProperty> properties;
	/**
	 * These properties require initialization when a new instance is created.
	 * This includes properties that are primitives as well as relationships
	 * requiring indirection ValueHolders or collections.
	 */
	private List<EntityProperty> propertiesRequiringInitialization;

	protected EntityType(ClassDescriptor descriptor) {
		if (descriptor.isAggregateDescriptor()) {
			throw DynamicEntityException
					.featureNotSupported("AggregateObjectMapping - "
							+ descriptor.getAlias());
		}
		if (descriptor.hasInheritance()) {
			throw DynamicEntityException.featureNotSupported("Inheritance - "
					+ descriptor.getAlias());
		}

		this.descriptor = descriptor;
		createProperties();

		descriptor.setObjectChangePolicy(new AttributeChangeTrackingPolicy());
		descriptor.getInstantiationPolicy().useFactoryInstantiationPolicy(this,
				FACTORY_METHOD);
	}

	public static EntityType getType(ClassDescriptor descriptor) {
		EntityType type = (EntityType) descriptor
				.getProperty(DESCRIPTOR_PROPERTY);

		if (type == null) {
			synchronized (descriptor) {
				type = (EntityType) descriptor.getProperty(DESCRIPTOR_PROPERTY);
				if (type == null) {
					type = new EntityType(descriptor);
					descriptor.setProperty(DESCRIPTOR_PROPERTY, type);
				}
			}
		}

		return type;
	}

	public ClassDescriptor getDescriptor() {
		return this.descriptor;
	}

	protected Map<String, EntityProperty> getPropertiesMap() {
		return this.propertiesMap;
	}

	protected List<EntityProperty> getProperties() {
		return this.properties;
	}

	public Iterator<EntityProperty> getPropertiesIterator() {
		return getProperties().iterator();
	}

	public String getName() {
		return getDescriptor().getAlias();
	}

	public int getPropertiesSize() {
		return getProperties().size();
	}

	public Set<String> getPropertiesNames() {
		return getPropertiesMap().keySet();
	}

	public boolean containsProperty(String propertyName) {
		return getPropertiesMap().containsKey(propertyName);
	}

	public EntityProperty getProperty(String propertyName) {
		EntityProperty prop = getPropertiesMap().get(propertyName);

		if (prop == null) {
			throw DynamicEntityException
					.invalidPropertyName(this, propertyName);
		}
		return prop;
	}

	public EntityProperty getProperty(int propertyIndex) {
		if (propertyIndex < 0 || propertyIndex > getProperties().size()) {
			throw DynamicEntityException.invalidPropertyIndex(this,
					propertyIndex);
		}
		return getProperties().get(propertyIndex);
	}

	public int getPropertyIndex(String propertyName) {
		return getProperty(propertyName).getIndex();
	}

	public Class getJavaClass() {
		return getDescriptor().getJavaClass();
	}
	protected void createProperties() {
		int numProperties = getDescriptor().getMappings().size();
		this.properties = new ArrayList<EntityProperty>(numProperties);
		this.propertiesMap = new HashMap<String, EntityProperty>(numProperties);
		this.propertiesRequiringInitialization = new ArrayList<EntityProperty>();

		for (int index = 0; index < numProperties; index++) {
			DatabaseMapping mapping = (DatabaseMapping) getDescriptor()
					.getMappings().get(index);
			EntityProperty property = buildProperty(mapping, index);

			this.properties.add(property);
			this.propertiesMap.put(property.getName(), property);

			if (property.requiresInitialization()) {
				getPropertiesRequiringInitialization().add(property);
			}
		}
	}

	private EntityProperty buildProperty(DatabaseMapping mapping, int index) {
		if (mapping.isForeignReferenceMapping()) {
			ForeignReferenceMapping frMapping = (ForeignReferenceMapping) mapping;

			if (frMapping.isCollectionMapping()) {
				return new EntityCollectionProperty(this,
						(CollectionMapping) mapping, index);
			}
			return new EntityReferenceProperty(this, (OneToOneMapping) mapping,
					index);
		}
		return new EntityProperty(this, mapping, index);

	}

	/**
	 * Invoked from EntityTypeFactory to augment an existing type/descriptor
	 * ONLY
	 */
	protected EntityProperty addProperty(DatabaseMapping mapping,
			Class attributeType) {
		int index = getDescriptor().getMappings().indexOf(mapping);

		EntityProperty entityProp = buildProperty(mapping, index);
		entityProp.setAttributeType(attributeType);

		this.properties.add(entityProp);
		this.propertiesMap.put(entityProp.getName(), entityProp);

		if (entityProp.requiresInitialization()) {
			getPropertiesRequiringInitialization().add(entityProp);
		}

		return entityProp;
	}

	public List<EntityProperty> getPropertiesRequiringInitialization() {
		return this.propertiesRequiringInitialization;
	}

	/**
	 * 
	 * @return new DynamicEntity with initialized attributes
	 */
	public DynamicEntity newInstance() {
		DynamicEntity entity = buildNewInstance(this);

		for (EntityProperty property : getPropertiesRequiringInitialization()) {
			property.initializeValue(entity);
		}

		return entity;
	}

	private Constructor defaultConstructor = null;

	/**
	 * Return the default (zero-argument) constructor for the descriptor class.
	 */
	protected Constructor getTypeConstructor() throws DescriptorException {
		// Lazy initialize, because the constructor cannot be serialized
		if (defaultConstructor == null) {
			buildTypeConstructorFor(getDescriptor().getJavaClass());
		}
		return defaultConstructor;
	}

	/**
	 * Build and return the default (zero-argument) constructor for the
	 * specified class.
	 */
	protected void buildTypeConstructorFor(Class javaClass)
			throws DescriptorException {
		try {
			this.defaultConstructor = PrivilegedAccessHelper
					.getDeclaredConstructorFor(javaClass,
							new Class[] { EntityType.class }, true);
		} catch (NoSuchMethodException exception) {
			throw DescriptorException
					.noSuchMethodWhileInitializingInstantiationPolicy(javaClass
							.getName()
							+ ".<Default Constructor>", getDescriptor(),
							exception);
		}
	}

	protected DynamicEntity buildNewInstance(EntityType type)
			throws DescriptorException {
		try {
			return (DynamicEntity) PrivilegedAccessHelper.invokeConstructor(
					this.getTypeConstructor(), new Object[] { type });
		} catch (InvocationTargetException exception) {
			throw DescriptorException
					.targetInvocationWhileConstructorInstantiation(this
							.getDescriptor(), exception);
		} catch (IllegalAccessException exception) {
			throw DescriptorException
					.illegalAccessWhileConstructorInstantiation(this
							.getDescriptor(), exception);
		} catch (InstantiationException exception) {
			throw DescriptorException
					.instantiationWhileConstructorInstantiation(this
							.getDescriptor(), exception);
		} catch (NoSuchMethodError exception) {
			// This exception is not documented but gets thrown.
			throw DescriptorException
					.noSuchMethodWhileConstructorInstantiation(this
							.getDescriptor(), exception);
		} catch (NullPointerException exception) {
			// Some JVMs will throw a NULL pointer exception here
			throw DescriptorException.nullPointerWhileConstructorInstantiation(
					this.getDescriptor(), exception);
		}
	}
}
