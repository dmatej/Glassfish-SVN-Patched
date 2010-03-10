/*******************************************************************************
 * Copyright (c) 1998, 2008 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     dclarke - Bug 277731: Simple Map Dynamic JPA Example
 *               http://wiki.eclipse.org/EclipseLink/Examples/JPA/Dynamic/SimpleDynamicMap
 ******************************************************************************/
package model;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.persistence.exceptions.DescriptorException;
import org.eclipse.persistence.mappings.AttributeAccessor;
import org.eclipse.persistence.mappings.DatabaseMapping;

/**
 * Simple Map base domain class for dynamic entities.
 * 
 * @author dclarke
 * @since EclipseLink 2.0
 */
public abstract class DynamicMapEntity extends HashMap<String, Object> {

    /**
     * Simple AttributeAccessor which retrieves a value from the DynamicEntity
     * using its Map interface. Since it is used in a dynamic case its
     * attributeClass must also be specified for schema generation to work.
     * 
     * This accessor must be configured on all mappings which access values out
     * of a DynamicEntity. Without this accessor EclipseLink will attempt to
     * look for a field on the class causing a validation exception to be
     * thrown.
     */
    public static class ValueAccessor extends AttributeAccessor {

        /** Owning mapping. Used to access attributeName */
        private DatabaseMapping mapping;

        /**
         * The attribute's type. Required for conversion out of result set and
         * schema generation
         */
        private Class attributeClass;

        public ValueAccessor(DatabaseMapping mapping, Class attributeClass) {
            super();
            this.mapping = mapping;
            this.attributeClass = attributeClass;
        }

        public DatabaseMapping getMapping() {
            return this.mapping;
        }

        public Object getAttributeValueFromObject(Object entity) throws DescriptorException {
            return ((Map<String, Object>) entity).get(getMapping().getAttributeName());
        }

        public void setAttributeValueInObject(Object entity, Object value) throws DescriptorException {
            ((Map<String, Object>) entity).put(getMapping().getAttributeName(), value);
        }

        @Override
        public Class getAttributeClass() {
            return this.attributeClass;
        }
    }
}
