/*******************************************************************************
 * Copyright (c) 2010-2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *  dclarke - EclipseLink 2.3 - MySports Demo Bug 344608
 ******************************************************************************/
package example.mysports.view;

import java.util.HashMap;
import java.util.Map;

import example.mysports.model.Extensible;

/**
 * {@link Extensible} type wrapper used in JSF pages to allow access to the
 * extended attributes through standard JSP EL.
 * <p>
 * Static Attribute: #{player.entity.firstName}
 * <p>
 * Extended Attribute: #{player.get('middleName').value}
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
public class ExtensibleEntity<T> {

    private Extensible entity;
    
    private Map<String, AttributeValue> values;

    public ExtensibleEntity(Extensible entity) {
        this.entity = entity;
        this.values = new HashMap<String, AttributeValue>();
    }

    @SuppressWarnings("unchecked")
    public T getEntity() {
        return (T) entity;
    }

    public AttributeValue get(String attributeName) {
        AttributeValue value = this.values.get(attributeName);
        if (value == null) {
            value = new AttributeValue(attributeName);
            this.values.put(attributeName, value);
        }
        return value;
    }

    public class AttributeValue {

        private String attribute;

        AttributeValue(String attribute) {
            this.attribute = attribute;
        }

        String getAttributeName() {
            return attribute;
        }

        public Object getValue() {
            return ExtensibleEntity.this.entity.get(getAttributeName());
        }

        public void setValue(Object value) {
            ExtensibleEntity.this.entity.set(getAttributeName(), value);
        }
    }

}
