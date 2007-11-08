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
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.sun.persistence.api.deployment.AssociationTableDescriptor;
import com.sun.persistence.api.deployment.ColumnDescriptor;
import com.sun.persistence.api.deployment.DescriptorNode;
import com.sun.persistence.api.deployment.EmbeddedDescriptor;
import com.sun.persistence.api.deployment.EmbeddedIdDescriptor;
import com.sun.persistence.api.deployment.IdDescriptor;
import com.sun.persistence.api.deployment.JoinColumnDescriptor;
import com.sun.persistence.api.deployment.MappingDescriptor;
import com.sun.persistence.api.deployment.NamedQueryDescriptor;
import com.sun.persistence.api.deployment.SequenceGeneratorDescriptor;
import com.sun.persistence.api.deployment.TableGeneratorDescriptor;

@XmlAccessorType(value = AccessType.FIELD)
@XmlType(name = "property", namespace = "http://java.sun.com/xml/ns/persistence_ORM")
public class PropertyDescriptor
    extends DescriptorNode
{

    @XmlElement(name = "mapping", namespace = "http://java.sun.com/xml/ns/persistence_ORM", type = MappingDescriptor.class)
    protected MappingDescriptor mapping;
    @XmlElement(name = "id", namespace = "http://java.sun.com/xml/ns/persistence_ORM", type = IdDescriptor.class)
    protected IdDescriptor id;
    @XmlElement(name = "embedded-id", namespace = "http://java.sun.com/xml/ns/persistence_ORM", type = EmbeddedIdDescriptor.class)
    protected EmbeddedIdDescriptor embeddedId;
    @XmlElement(name = "embedded", namespace = "http://java.sun.com/xml/ns/persistence_ORM", type = EmbeddedDescriptor.class)
    protected EmbeddedDescriptor embedded;
    @XmlElement(defaultValue = "false", name = "version", namespace = "http://java.sun.com/xml/ns/persistence_ORM", type = Boolean.class)
    protected Boolean version;
    @XmlElement(name = "association-table", namespace = "http://java.sun.com/xml/ns/persistence_ORM", type = AssociationTableDescriptor.class)
    protected AssociationTableDescriptor associationTable;
    @XmlElement(name = "named-query", namespace = "http://java.sun.com/xml/ns/persistence_ORM", type = NamedQueryDescriptor.class)
    protected List<NamedQueryDescriptor> namedQuery;
    @XmlElement(name = "column", namespace = "http://java.sun.com/xml/ns/persistence_ORM", type = ColumnDescriptor.class)
    protected ColumnDescriptor column;
    @XmlElement(name = "join-column", namespace = "http://java.sun.com/xml/ns/persistence_ORM", type = JoinColumnDescriptor.class)
    protected List<JoinColumnDescriptor> joinColumn;
    @XmlElement(name = "sequence-generator", namespace = "http://java.sun.com/xml/ns/persistence_ORM", type = SequenceGeneratorDescriptor.class)
    protected SequenceGeneratorDescriptor sequenceGenerator;
    @XmlElement(name = "table-generator", namespace = "http://java.sun.com/xml/ns/persistence_ORM", type = TableGeneratorDescriptor.class)
    protected TableGeneratorDescriptor tableGenerator;
    @XmlAttribute(name = "name", namespace = "")
    protected String name;

    /**
     * Gets the value of the mapping property.
     * 
     * @return
     *     possible object is
     *     {@link com.sun.persistence.api.deployment.MappingDescriptor}
     */
    public MappingDescriptor getMapping() {
        return mapping;
    }

    /**
     * Sets the value of the mapping property.
     * 
     * @param value
     *     allowed object is
     *     {@link com.sun.persistence.api.deployment.MappingDescriptor}
     */
    public void setMapping(MappingDescriptor value) {
        this.mapping = value;
        value.parent(this);
    }

    public boolean isSetMapping() {
        return (this.mapping!= null);
    }

    public void unsetMapping() {
        this.mapping = null;
    }

    /**
     * Gets the value of the id property.
     * 
     * @return
     *     possible object is
     *     {@link com.sun.persistence.api.deployment.IdDescriptor}
     */
    public IdDescriptor getId() {
        return id;
    }

    /**
     * Sets the value of the id property.
     * 
     * @param value
     *     allowed object is
     *     {@link com.sun.persistence.api.deployment.IdDescriptor}
     */
    public void setId(IdDescriptor value) {
        this.id = value;
        value.parent(this);
    }

    public boolean isSetId() {
        return (this.id!= null);
    }

    public void unsetId() {
        this.id = null;
    }

    /**
     * Gets the value of the embeddedId property.
     * 
     * @return
     *     possible object is
     *     {@link com.sun.persistence.api.deployment.EmbeddedIdDescriptor}
     */
    public EmbeddedIdDescriptor getEmbeddedId() {
        return embeddedId;
    }

    /**
     * Sets the value of the embeddedId property.
     * 
     * @param value
     *     allowed object is
     *     {@link com.sun.persistence.api.deployment.EmbeddedIdDescriptor}
     */
    public void setEmbeddedId(EmbeddedIdDescriptor value) {
        this.embeddedId = value;
        value.parent(this);
    }

    public boolean isSetEmbeddedId() {
        return (this.embeddedId!= null);
    }

    public void unsetEmbeddedId() {
        this.embeddedId = null;
    }

    /**
     * Gets the value of the embedded property.
     * 
     * @return
     *     possible object is
     *     {@link com.sun.persistence.api.deployment.EmbeddedDescriptor}
     */
    public EmbeddedDescriptor getEmbedded() {
        return embedded;
    }

    /**
     * Sets the value of the embedded property.
     * 
     * @param value
     *     allowed object is
     *     {@link com.sun.persistence.api.deployment.EmbeddedDescriptor}
     */
    public void setEmbedded(EmbeddedDescriptor value) {
        this.embedded = value;
        value.parent(this);
    }

    public boolean isSetEmbedded() {
        return (this.embedded!= null);
    }

    public void unsetEmbedded() {
        this.embedded = null;
    }

    /**
     * Gets the value of the version property.
     * 
     * @return
     *     possible object is
     *     {@link java.lang.Boolean}
     */
    public Boolean isVersion() {
        return version;
    }

    /**
     * Sets the value of the version property.
     * 
     * @param value
     *     allowed object is
     *     {@link java.lang.Boolean}
     */
    public void setVersion(Boolean value) {
        this.version = value;
    }

    public boolean isSetVersion() {
        return (this.version!= null);
    }

    public void unsetVersion() {
        this.version = null;
    }

    /**
     * Gets the value of the associationTable property.
     * 
     * @return
     *     possible object is
     *     {@link com.sun.persistence.api.deployment.AssociationTableDescriptor}
     */
    public AssociationTableDescriptor getAssociationTable() {
        return associationTable;
    }

    /**
     * Sets the value of the associationTable property.
     * 
     * @param value
     *     allowed object is
     *     {@link com.sun.persistence.api.deployment.AssociationTableDescriptor}
     */
    public void setAssociationTable(AssociationTableDescriptor value) {
        this.associationTable = value;
        value.parent(this);
    }

    public boolean isSetAssociationTable() {
        return (this.associationTable!= null);
    }

    public void unsetAssociationTable() {
        this.associationTable = null;
    }

    protected List<NamedQueryDescriptor> _getNamedQuery() {
        if (namedQuery == null) {
            namedQuery = new ArrayList<NamedQueryDescriptor>();
        }
        return namedQuery;
    }

    /**
     * Gets the value of the namedQuery property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the namedQuery property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getNamedQuery().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link com.sun.persistence.api.deployment.NamedQueryDescriptor}
     * 
     */
    public List<NamedQueryDescriptor> getNamedQuery() {
        return this._getNamedQuery();
    }

    public boolean isSetNamedQuery() {
        return (this.namedQuery!= null);
    }

    public void unsetNamedQuery() {
        this.namedQuery = null;
    }

    /**
     * Gets the value of the column property.
     * 
     * @return
     *     possible object is
     *     {@link com.sun.persistence.api.deployment.ColumnDescriptor}
     */
    public ColumnDescriptor getColumn() {
        return column;
    }

    /**
     * Sets the value of the column property.
     * 
     * @param value
     *     allowed object is
     *     {@link com.sun.persistence.api.deployment.ColumnDescriptor}
     */
    public void setColumn(ColumnDescriptor value) {
        this.column = value;
        value.parent(this);
    }

    public boolean isSetColumn() {
        return (this.column!= null);
    }

    public void unsetColumn() {
        this.column = null;
    }

    protected List<JoinColumnDescriptor> _getJoinColumn() {
        if (joinColumn == null) {
            joinColumn = new ArrayList<JoinColumnDescriptor>();
        }
        return joinColumn;
    }

    /**
     * Gets the value of the joinColumn property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the joinColumn property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getJoinColumn().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link com.sun.persistence.api.deployment.JoinColumnDescriptor}
     * 
     */
    public List<JoinColumnDescriptor> getJoinColumn() {
        return this._getJoinColumn();
    }

    public boolean isSetJoinColumn() {
        return (this.joinColumn!= null);
    }

    public void unsetJoinColumn() {
        this.joinColumn = null;
    }

    /**
     * Gets the value of the sequenceGenerator property.
     * 
     * @return
     *     possible object is
     *     {@link com.sun.persistence.api.deployment.SequenceGeneratorDescriptor}
     */
    public SequenceGeneratorDescriptor getSequenceGenerator() {
        return sequenceGenerator;
    }

    /**
     * Sets the value of the sequenceGenerator property.
     * 
     * @param value
     *     allowed object is
     *     {@link com.sun.persistence.api.deployment.SequenceGeneratorDescriptor}
     */
    public void setSequenceGenerator(SequenceGeneratorDescriptor value) {
        this.sequenceGenerator = value;
        value.parent(this);
    }

    public boolean isSetSequenceGenerator() {
        return (this.sequenceGenerator!= null);
    }

    public void unsetSequenceGenerator() {
        this.sequenceGenerator = null;
    }

    /**
     * Gets the value of the tableGenerator property.
     * 
     * @return
     *     possible object is
     *     {@link com.sun.persistence.api.deployment.TableGeneratorDescriptor}
     */
    public TableGeneratorDescriptor getTableGenerator() {
        return tableGenerator;
    }

    /**
     * Sets the value of the tableGenerator property.
     * 
     * @param value
     *     allowed object is
     *     {@link com.sun.persistence.api.deployment.TableGeneratorDescriptor}
     */
    public void setTableGenerator(TableGeneratorDescriptor value) {
        this.tableGenerator = value;
        value.parent(this);
    }

    public boolean isSetTableGenerator() {
        return (this.tableGenerator!= null);
    }

    public void unsetTableGenerator() {
        this.tableGenerator = null;
    }

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

    //Added code

    PropertyDescriptor() {
        namedQuery = new com.sun.persistence.api.deployment.DescriptorNodeList<NamedQueryDescriptor>(this);
        joinColumn = new com.sun.persistence.api.deployment.DescriptorNodeList<JoinColumnDescriptor>(this);
    }
    public void accept(Visitor v) throws DeploymentException {
        v.visitPropertyDescriptor(this);
    }

    public boolean isRelationshipProperty() {
        return getMapping() instanceof RelationalMappingDescriptor;
    }

    @Override public ClassDescriptor parent() {
        return ClassDescriptor.class.cast(super.parent());
    }

    public boolean isId(){
        return getId()!=null || getEmbeddedId()!=null;
    }

    public boolean isEmbedded() {
        return getEmbedded() != null || getEmbeddedId() != null;
    }
}
