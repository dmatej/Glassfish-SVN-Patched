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
 *     10/02/2008-1.1M1 Michael O'Brien 
 *       - 250473: This DDL application managed DDL generation application is required before running the  
 *       tutorial submissions for WebLogic, JBoss, OC4J, GlassFish, Tomcat and WebSphere 
 *       - this project is specific to the Oracle database - customization via persistence.xml is possible for other databases.
 *       see
 *       http://wiki.eclipse.org/EclipseLink/Examples/JPA#JPA_Web_Application_Tutorials
 ******************************************************************************/

package org.eclipse.persistence.example.jpa.server.business;

import java.io.Serializable;
import java.math.BigInteger;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import javax.persistence.OneToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

/**
 * This simplified entity class is used to illustrate JPA concepts in stand-alone, web or ejb containers
 * @author mfobrien
 */
@Entity
@Table(name="EL_CELL")
public class Cell implements Serializable {
    
    @Id
    // keep the sequence column name under 30 chars to avoid an ORA-00972   
    @SequenceGenerator(name="EL_SEQUENCE_CELL", sequenceName="EL_CELL_SEQ", allocationSize=25)
    @GeneratedValue(generator="EL_SEQUENCE_CELL")
    private BigInteger id;

    // usually read-only
    @Column(name="TSEQ")
    private String transitionSequence;
    
    public String getTransitionSequence() {
        return transitionSequence;
    }

    public void setTransitionSequence(String transitionSequence) {
        this.transitionSequence = transitionSequence;
    }

    public String getState() {
        return state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public Set<Cell> getPeers() {
        return peers;
    }

    public void setPeers(Set<Cell> peers) {
        this.peers = peers;
    }

    public Set<Cell> getReferences() {
        return references;
    }

    public void setReferences(Set<Cell> references) {
        this.references = references;
    }

    @Column(name="STATE")
    private String state;
    
    // inverse relationship
    // Force loading of entire tree structure
    //@JoinColumn(name="REFERENCES")
    // m-m and 1-m do not weave by default
    @ManyToMany(mappedBy="references", fetch=FetchType.EAGER)
    private Set<Cell> peers;
    
    // owning relationship
    //@JoinColumn(name="PEERS")    
    //@ManyToMany(mappedBy="peers", fetch=FetchType.EAGER)    
    @ManyToMany(fetch=FetchType.EAGER)
    private Set<Cell>  references;
    
    // Weaving is not supported for manyToMany above, we introduce a single oneToOne here
    // inverse relationship
    @OneToOne(mappedBy="right", fetch=FetchType.EAGER)
    private Cell left;

    public Cell getLeft() {
        return left;
    }

    public void setLeft(Cell left) {
        this.left = left;
    }

    public Cell getRight() {
        return right;
    }

    public void setRight(Cell right) {
        this.right = right;
    }

    // owning relationship
    //@OneToOne(mappedBy="left", fetch=FetchType.EAGER)
    @OneToOne(fetch=FetchType.EAGER)    
    private Cell right;
    
    public BigInteger getId() {
        return id;
    }

    public void setId(BigInteger id) {
        this.id = id;
    }

    /**
     * Override toString() to print out the fields of the Entity during logging
     */
    public String toString() {
        StringBuffer aBuffer = new StringBuffer(getClass().getName());
        aBuffer.append("@");
        aBuffer.append(hashCode());
        aBuffer.append("( id: ");
        aBuffer.append(getId());
        aBuffer.append(" state: ");
        aBuffer.append(getState());
        aBuffer.append(" left: ");
        aBuffer.append(getLeft());
        aBuffer.append(" right: ");
        aBuffer.append(getRight());
        aBuffer.append(" parent: ");
        // we want the short form of embedded objects
        if(null == peers) {
            aBuffer.append(getPeers());
        } else {
            aBuffer.append(getPeers().getClass().getSimpleName());
            aBuffer.append("@");
            aBuffer.append(getPeers().hashCode());
        }
        aBuffer.append(" references: ");
        if(null == references) {
            aBuffer.append(getReferences());
        } else {
            aBuffer.append(getReferences().getClass().getSimpleName());
            aBuffer.append("@");
            aBuffer.append(getReferences().hashCode());
        }
        aBuffer.append(")");
        return aBuffer.toString();
    }    
}
