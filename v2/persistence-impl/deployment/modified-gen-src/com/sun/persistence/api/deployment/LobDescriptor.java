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

import javax.xml.bind.annotation.AccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.sun.persistence.api.deployment.FetchType;
import com.sun.persistence.api.deployment.LobType;
import com.sun.persistence.api.deployment.MappingDescriptor;

@XmlAccessorType(value = AccessType.FIELD)
@XmlType(name = "lob", namespace = "http://java.sun.com/xml/ns/persistence_ORM")
public class LobDescriptor
    extends MappingDescriptor
{

    @XmlElement(defaultValue = "LAZY", name = "fetch", namespace = "http://java.sun.com/xml/ns/persistence_ORM", type = FetchType.class)
    protected FetchType fetch;
    @XmlElement(defaultValue = "BLOB", name = "type", namespace = "http://java.sun.com/xml/ns/persistence_ORM", type = LobType.class)
    protected LobType type;

    /**
     * Gets the value of the fetch property.
     * 
     * @return
     *     possible object is
     *     {@link com.sun.persistence.api.deployment.FetchType}
     */
    public FetchType getFetch() {
        return fetch;
    }

    /**
     * Sets the value of the fetch property.
     * 
     * @param value
     *     allowed object is
     *     {@link com.sun.persistence.api.deployment.FetchType}
     */
    public void setFetch(FetchType value) {
        this.fetch = value;
    }

    public boolean isSetFetch() {
        return (this.fetch!= null);
    }

    public void unsetFetch() {
        this.fetch = null;
    }

    /**
     * Gets the value of the type property.
     * 
     * @return
     *     possible object is
     *     {@link com.sun.persistence.api.deployment.LobType}
     */
    public LobType getType() {
        return type;
    }

    /**
     * Sets the value of the type property.
     * 
     * @param value
     *     allowed object is
     *     {@link com.sun.persistence.api.deployment.LobType}
     */
    public void setType(LobType value) {
        this.type = value;
    }

    public boolean isSetType() {
        return (this.type!= null);
    }

    public void unsetType() {
        this.type = null;
    }

    //Added code

    LobDescriptor() {
    }

    public void accept(Visitor v) throws DeploymentException {
        v.visitLobDescriptor(this);
    }
    
}
