/*
 * The contents of this file are subject to the terms 
 * of the Common Development and Distribution License 
 * (the "License").  You may not use this file except 
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at 
 * glassfish/bootstrap/legal/CDDLv1.0.txt or 
 * https://glassfish.dev.java.net/public/CDDLv1.0.html. 
 * See the License for the specific language governing 
 * permissions and limitations under the License.
 * 
 * When distributing Covered Code, include this CDDL 
 * HEADER in each file and include the License file at 
 * glassfish/bootstrap/legal/CDDLv1.0.txt.  If applicable, 
 * add the following below this CDDL HEADER, with the 
 * fields enclosed by brackets "[]" replaced with your 
 * own identifying information: Portions Copyright [yyyy] 
 * [name of copyright owner]
 */

//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, vhudson-jaxb-1973 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// This file may need to modified upon recompilation of the source schema. 
// Generated on: 2005.04.20 at 08:27:00 IST 
//


package com.sun.persistence.api.deployment;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.AccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.sun.persistence.api.deployment.AttributeOverrideDescriptor;
import com.sun.persistence.api.deployment.DescriptorNode;

@XmlAccessorType(value = AccessType.FIELD)
@XmlType(name = "embedded", namespace = "http://java.sun.com/xml/ns/persistence_ORM")
public class EmbeddedDescriptor
    extends DescriptorNode
{

    @XmlElement(name = "value", namespace = "http://java.sun.com/xml/ns/persistence_ORM", type = AttributeOverrideDescriptor.class)
    protected List<AttributeOverrideDescriptor> value;

    protected List<AttributeOverrideDescriptor> _getValue() {
        if (value == null) {
            value = new ArrayList<AttributeOverrideDescriptor>();
        }
        return value;
    }

    /**
     * Gets the value of the value property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the value property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getValue().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link com.sun.persistence.api.deployment.AttributeOverrideDescriptor}
     * 
     */
    public List<AttributeOverrideDescriptor> getValue() {
        return this._getValue();
    }

    public boolean isSetValue() {
        return (this.value!= null);
    }

    public void unsetValue() {
        this.value = null;
    }

    //Added code

    EmbeddedDescriptor() {
        value = new com.sun.persistence.api.deployment.DescriptorNodeList<AttributeOverrideDescriptor>(this);
    }
    public void accept(Visitor v) throws DeploymentException {
        v.visitEmbeddedDescriptor(this);
    }

    @Override public PropertyDescriptor parent() {
        return PropertyDescriptor.class.cast(super.parent());
    }

}
