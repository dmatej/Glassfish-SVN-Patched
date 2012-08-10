/*******************************************************************************
 * Copyright (c) 2010, 2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     16/02/2011 2.3  Michael O'Brien 
 *          - 337037: initial API and implementation platform to be used for 
 *             distributed EE application research, development and architecture
 ******************************************************************************/  
package org.eclipse.persistence.example.distributed.collatz.model;

import java.io.Serializable;
import java.math.BigInteger;

import javax.persistence.Basic;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.Version;

import org.eclipse.persistence.annotations.Convert;
import org.eclipse.persistence.annotations.TypeConverter;

/**
 * This class is part of a distributed application framework used to simulate and research
 * concurrency, analytics, management, performance and exception handling.
 * The focus is on utilizing JPA 2.0 as the persistence layer for scenarios involving
 * multicore, multithreaded and multiuser distributed memory L1 persistence applications.
 * The secondary focus is on exercising Java EE6 API to access the results of this distributed application.
 * 
 * @see http://bugs.eclipse.org/337037
 * @see http://wiki.eclipse.org/EclipseLink/Examples/Distributed
 * @author Michael O'Brien
 * @since EclipseLink 2.3
 */
@Entity
@TypeConverter(name="BigIntegerToString",dataType=String.class,objectType=BigInteger.class)
public class CollatzRecord implements Serializable {
    private static final long serialVersionUID = 4023830926240714638L;
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    @ManyToOne(fetch=FetchType.EAGER)
    private UnitOfWork unitOfWork;
    @Column(nullable=false, length=512)
    @Convert("BigIntegerToString")
    private BigInteger initial;
    @Column(nullable=false, length=512)
    @Convert("BigIntegerToString")
    private BigInteger pathLength;
    @Column(nullable=false, length=512)
    @Convert("BigIntegerToString")
    private BigInteger maximum;
    /** A value of true means this is a maximum value record (independent of max path) */
    @Basic
    private boolean isMaxRecord;
    /** A value of true means this is a maximum path record (independent of max value) */
    @Basic
    private boolean isPathRecord;
    
    @Version
    private Long version;

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (id != null ? id.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof CollatzRecord)) {
            return false;
        }
        CollatzRecord other = (CollatzRecord) object;
        if ((this.id == null && other.id != null) || (this.id != null && !this.id.equals(other.id))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        StringBuffer aBuffer = new StringBuffer(getClass().getSimpleName());
        aBuffer.append("@");
        aBuffer.append(hashCode());
        aBuffer.append("( id: ");
        aBuffer.append(getId());
        aBuffer.append(")");
        return aBuffer.toString();
    }    

    public Long getId() {        return id;   }
    public void setId(Long id) {        this.id = id;    }
    public BigInteger getInitial() {        return initial;    }
    public void setInitial(BigInteger initial) {        this.initial = initial;    }
    public BigInteger getPathLength() {        return pathLength;    }
    public void setPathLength(BigInteger pathLength) {        this.pathLength = pathLength;    }
    public BigInteger getMaximum() {        return maximum;    }
    public void setMaximum(BigInteger maximum) {        this.maximum = maximum;    }
    public Long getVersion() {        return version;    }
    public void setVersion(Long version) {        this.version = version;    }
    public UnitOfWork getUnitOfWork() {        return unitOfWork;    }
    public void setUnitOfWork(UnitOfWork unitOfWork) {        this.unitOfWork = unitOfWork;    }
    public boolean isMaxRecord() {    	return isMaxRecord;    }    
    public boolean isPathRecord() {    	return isPathRecord;    }
    public void setIsMaxRecord(boolean max) {    	isMaxRecord = max;    }
    public void setIsPathRecord(boolean path) {    	isPathRecord = path;    }
}
