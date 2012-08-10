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
 *     09/03/2011 2.3  Michael O'Brien 
 *          - 337037: initial API and implementation platform to be used for 
 *             distributed EE application research, development and architecture
 ******************************************************************************/  
package org.eclipse.persistence.example.distributed.collatz.model;

import java.io.Serializable;
import java.math.BigInteger;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToOne;
import javax.persistence.Version;

import org.eclipse.persistence.annotations.Convert;
import org.eclipse.persistence.annotations.TypeConverter;
import org.eclipse.persistence.annotations.TypeConverters;

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
@TypeConverters({@TypeConverter(name="BigIntegerToString",dataType=String.class,objectType=BigInteger.class)})
public class ComputeGrid implements Serializable {
    private static final long serialVersionUID = 5273909837574142903L;

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;

    // Note: These two may be the same record
    @OneToOne // unidirectional OneToOne
    private CollatzRecord maxPathRecord;
    @OneToOne // unidirectional OneToOne    
    private CollatzRecord maxValueRecord;
    
    @Column(length=512)
    @Convert("BigIntegerToString")
    private BigInteger nextNumberToSearch;
    private Long globalStartTimestamp;
    @Column(name="globalduration", length=512)
    @Convert("BigIntegerToString")
    private BigInteger globalDuration;
    @Column(name="operations",nullable=false,length=512) // nullable to avoid /0 error
    //@Column(nullable=false,length=512) // nullable to avoid /0 error
    @Convert("BigIntegerToString")
    private BigInteger operations;

	@Column(name="bestIterationsPerSecond", length=512)
    @Convert("BigIntegerToString")
    private BigInteger bestIterationsPerSecond;
    @Column(name="partitionLength", length=512)
    @Convert("BigIntegerToString")
    private BigInteger partitionLength;
    
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
        if (!(object instanceof ComputeGrid)) {
            return false;
        }
        ComputeGrid other = (ComputeGrid) object;
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

    // Composite get/set
    public BigInteger getMaxPath() {        return getMaxPathRecord().getPathLength();    }
    public BigInteger getMaxValue() {        return getMaxValueRecord().getMaximum();    }

    // Simple get/set
    public CollatzRecord getMaxPathRecord() {        return maxPathRecord;    }
    public void setMaxPathRecord(CollatzRecord maxPathRecord) {        this.maxPathRecord = maxPathRecord;    }
    public CollatzRecord getMaxValueRecord() {        return maxValueRecord;    }
    public void setMaxValueRecord(CollatzRecord maxValueRecord) {        this.maxValueRecord = maxValueRecord;    }    
    public Long getId() {        return id;    }
    public void setId(Long id) {        this.id = id;    }
    public BigInteger getNextNumberToSearch() {        return nextNumberToSearch;    }
    public void setNextNumberToSearch(BigInteger nextNumberToSearch) {        this.nextNumberToSearch = nextNumberToSearch;    }
    public Long getGlobalStartTimestamp() {        return globalStartTimestamp;    }
    public void setGlobalStartTimestamp(Long globalStartTimestamp) {        this.globalStartTimestamp = globalStartTimestamp;    }
    public BigInteger getGlobalDuration() {        return globalDuration;    }
    public void setGlobalDuration(BigInteger globalDuration) {        this.globalDuration = globalDuration;    }
    public BigInteger getBestIterationsPerSecond() {        return bestIterationsPerSecond;    }
    public void setBestIterationsPerSecond(BigInteger bestIterationsPerSecond) {        this.bestIterationsPerSecond = bestIterationsPerSecond;    }
    public Long getVersion() {        return version;    }
    public void setVersion(Long version) {        this.version = version;    }
    public BigInteger getPartitionLength() {        return partitionLength;    }
    public void setPartitionLength(BigInteger partitionLength) {        this.partitionLength = partitionLength;    }
    public BigInteger getOperations() {		return operations;	}
	public void setOperations(BigInteger operations) {		this.operations = operations;	}
}
