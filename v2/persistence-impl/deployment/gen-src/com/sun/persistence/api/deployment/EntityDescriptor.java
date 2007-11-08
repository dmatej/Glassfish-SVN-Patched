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
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2005.04.20 at 08:27:00 IST 
//


package com.sun.persistence.api.deployment;

import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import com.sun.persistence.api.deployment.Adapter1;
import com.sun.persistence.api.deployment.DescriptorNode;
import com.sun.persistence.api.deployment.EntityType;

@XmlAccessorType(value = javax.xml.bind.annotation.AccessType.FIELD)
@XmlType(name = "entity", namespace = "http://java.sun.com/xml/ns/persistence_ORM")
public class EntityDescriptor
    extends DescriptorNode
{

    @XmlElement(defaultValue = "", name = "name", namespace = "http://java.sun.com/xml/ns/persistence_ORM", type = String.class)
    protected String name;
    @XmlElement(defaultValue = "CMP", name = "entity-type", namespace = "http://java.sun.com/xml/ns/persistence_ORM", type = EntityType.class)
    protected EntityType entityType;
    @XmlElement(defaultValue = "PROPERTY", name = "access", namespace = "http://java.sun.com/xml/ns/persistence_ORM", type = com.sun.persistence.api.deployment.AccessType.class)
    protected com.sun.persistence.api.deployment.AccessType access;
    @XmlElement(defaultValue = "3", name = "version", namespace = "http://java.sun.com/xml/ns/persistence_ORM", type = String.class)
    @XmlJavaTypeAdapter(value = Adapter1 .class)
    protected Integer version;

    /**
     * Gets the value of the name property.
     * 
     * @return
     *     possible object is
     *     {@link java.lang.String}
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the value of the name property.
     * 
     * @param value
     *     allowed object is
     *     {@link java.lang.String}
     */
    public void setName(String value) {
        this.name = value;
    }

    public boolean isSetName() {
        return (this.name!= null);
    }

    public void unsetName() {
        this.name = null;
    }

    /**
     * Gets the value of the entityType property.
     * 
     * @return
     *     possible object is
     *     {@link com.sun.persistence.api.deployment.EntityType}
     */
    public EntityType getEntityType() {
        return entityType;
    }

    /**
     * Sets the value of the entityType property.
     * 
     * @param value
     *     allowed object is
     *     {@link com.sun.persistence.api.deployment.EntityType}
     */
    public void setEntityType(EntityType value) {
        this.entityType = value;
    }

    public boolean isSetEntityType() {
        return (this.entityType!= null);
    }

    public void unsetEntityType() {
        this.entityType = null;
    }

    /**
     * Gets the value of the access property.
     * 
     * @return
     *     possible object is
     *     {@link com.sun.persistence.api.deployment.AccessType}
     */
    public com.sun.persistence.api.deployment.AccessType getAccess() {
        return access;
    }

    /**
     * Sets the value of the access property.
     * 
     * @param value
     *     allowed object is
     *     {@link com.sun.persistence.api.deployment.AccessType}
     */
    public void setAccess(com.sun.persistence.api.deployment.AccessType value) {
        this.access = value;
    }

    public boolean isSetAccess() {
        return (this.access!= null);
    }

    public void unsetAccess() {
        this.access = null;
    }

    /**
     * Gets the value of the version property.
     * 
     * @return
     *     possible object is
     *     {@link java.lang.String}
     */
    public Integer getVersion() {
        return version;
    }

    /**
     * Sets the value of the version property.
     * 
     * @param value
     *     allowed object is
     *     {@link java.lang.String}
     */
    public void setVersion(Integer value) {
        this.version = value;
    }

    public boolean isSetVersion() {
        return (this.version!= null);
    }

    public void unsetVersion() {
        this.version = null;
    }

}
