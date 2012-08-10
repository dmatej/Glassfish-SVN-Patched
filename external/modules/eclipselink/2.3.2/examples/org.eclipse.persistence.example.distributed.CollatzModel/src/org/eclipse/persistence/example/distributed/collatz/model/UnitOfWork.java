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
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Transient;
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
public class UnitOfWork implements Serializable {
    private static final long serialVersionUID = 3287861579472326552L;
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    @OneToMany(mappedBy="unitOfWork", fetch=FetchType.EAGER,cascade=CascadeType.ALL)
    // record are an ordered list
    private List<CollatzRecord> records;
    
    @OneToOne(cascade=CascadeType.ALL)
    private ActiveProcessor processor;
    @Column(nullable=false, length=512)
    @Convert("BigIntegerToString")
    private BigInteger initial;
    @Column(nullable=false, length=512)
    @Convert("BigIntegerToString")
    private BigInteger extent;
    @OneToOne
    private CollatzRecord knownMax;
    @OneToOne
    private CollatzRecord knownPath;
    private Long startTimestamp;
    private Long endTimestamp;
    @Column(nullable=false) // nullable to avoid /0 error
    private long operations;
    
    private Integer retries;
    
    // Note: These two may be the same record
    @OneToOne // unidirectional OneToOne
    private CollatzRecord maxPathRecord;
    @OneToOne // unidirectional OneToOne    
    private CollatzRecord maxValueRecord;
    
    @Version
    private Long version;
    
    // Cached values - do not persist
    @Transient
    private BigInteger interval;

    public static final BigInteger TWO = BigInteger.valueOf(2);
    public static final BigInteger FOUR = BigInteger.valueOf(4);
    public static final long R2000 = 67457283406188652L;

    // business methods (separate from DTO methods)
    public long processInterval() {
        StringBuffer buffer = null;
        long operations = 0L;        
        BigInteger pathIteration = BigInteger.ONE;//, path should fit in 64 bits
        BigInteger maxValueIteration = BigInteger.ONE;
        boolean milestone = false;
        String prefix = null;
        //System.out.println("_collatz: " + System.currentTimeMillis() + ": threads,Interval,start,end:    " + processor.getThreads() + "," + getInterval() + "," + initial + "," + extent);
        List<BigInteger> list = new ArrayList<BigInteger>();        
        BigInteger currentNumber = initial;
        CollatzRecord record = null;
        setStartTimestamp(System.currentTimeMillis());
        while (currentNumber.compareTo(extent) < 0) {
        	list = hailstoneSequenceUsingBigInteger(list, currentNumber);
            // cache maxPath and maxValue for performance : were doing 10000 times the iterations required
            if(!list.isEmpty()) {
                maxValueIteration = list.remove(0);
                pathIteration = list.remove(0);
                operations = operations + pathIteration.longValue();
                // keep track of local milestones
                if(pathIteration.compareTo(getMaxPath()) > 0) {                    
                    milestone = true;
                    prefix = "P";
                    record = new CollatzRecord();
                    record.setPathLength(pathIteration);
                    record.setMaximum(maxValueIteration);
                    //setMaxPath(getMaxPath());
                    setMaxPathRecord(record);
                    record.setIsPathRecord(true);
                    // update cache
                    //maxPathLocal = pathIteration;
                }
                if(maxValueIteration.compareTo(getMaxValue()) > 0) {                    
                    if(milestone) {
                        prefix = "PM";
                    } else {
                        prefix = "M";
                        record = new CollatzRecord();
                        record.setPathLength(pathIteration);
                        record.setMaximum(maxValueIteration);
                        milestone = true;
                    }
                    setMaxValueRecord(record);
                    record.setIsMaxRecord(true);
                }
                if(milestone) {
                    record.setUnitOfWork(this);
                    record.setInitial(currentNumber);
                    addRecord(record);
                    buffer = new StringBuffer("_collatz: ");
                    buffer.append(System.currentTimeMillis());
                    buffer.append(": ");
                    buffer.append(getProcessor().getIdentifier());
                    buffer.append(": ");
                    buffer.append(prefix);
                    buffer.append(",");
                    buffer.append(this.getInterval());
                    buffer.append(",");
                    buffer.append(currentNumber);
                    buffer.append(",");
                    buffer.append(getMaxPath());// + PATH_OFFSET_FOUR); // we stop the search at 4 (so add a path of 2)
                    buffer.append("\t");
                    buffer.append(",");
                    buffer.append(maxValueIteration); // BigInteger implements Comparable
                    buffer.append("\t");
                    if((maxValueIteration.compareTo(BigInteger.valueOf(Long.MAX_VALUE)) > 0)) {
                        buffer.append("2^63+,");
                        buffer.append(maxValueIteration.subtract(BigInteger.valueOf(Long.MAX_VALUE)));
                    } else {
                        buffer.append(",");
                    }
                    System.out.println(buffer.toString());
                    milestone = false;
                }
            }
            // increment
            currentNumber = currentNumber.add(TWO);
        }
        setEndTimestamp(System.currentTimeMillis());
        setOperations(operations);
        return operations;   
    }

    /**
     * Return the range between the start and end points for this UnitOfWork packet
     * @return
     */
    public BigInteger getInterval() {
        if(null == interval) {
            // lazy load the interval
            interval = extent.subtract(initial);
        }
        return interval;
    }

    /**
     * Compute the hailstone (collatz) sequence for the start BigInteger.
     * Return a List of the maximum value and maximum path in this iteration.
     * @param list
     * @param start
     * @return
     */
    public List<BigInteger> hailstoneSequenceUsingBigInteger(List<BigInteger> list, BigInteger start)  {
    	BigInteger max = start;;
    	long path = 1;// - 0;//PATH_OFFSET_FOUR;
    	if(start.equals(BigInteger.ZERO) || start.equals(BigInteger.ONE)) {
    	    list.add(max);
    	    list.add(BigInteger.valueOf(path));
    	    return list;
    	}
    	BigInteger current = start;
    	while (current.compareTo(FOUR) > 0) { // Perf
    	    if(current.testBit(0)) { // test odd
    	        current = current.shiftLeft(1).setBit(0).add(current);
    	    } else {
    	        current = current.shiftRight(1);
    	    }
    	    // check max
    	    if(max.compareTo(current) < 0) {
    	        max = current;
    	    }
    	    path += 1;
    	}
    	list.add(max);
    	list.add(BigInteger.valueOf(path));
    	return list;
    }

    // Note: this function will overflow at 64 bits
    public List<BigInteger> hailstoneSequenceUsingLong(List<BigInteger> list, BigInteger start)  {
        BigInteger max = start;;
        long path = 1;// - 0;//PATH_OFFSET_FOUR;
        if(start.equals(BigInteger.ZERO) || start.equals(BigInteger.ONE)) {
            list.add(max);
            list.add(BigInteger.valueOf(path));
            return list;
        }
        BigInteger current = start;
        while (current.compareTo(FOUR) > 0) { // Perf
            if(current.testBit(0)) { // test odd
                current = current.shiftLeft(1).setBit(0).add(current);
            } else {
                current = current.shiftRight(1);
            }
            // check max
            if(max.compareTo(current) < 0) {
                max = current;
            }
            path += 1;
        }
        list.add(max);
        list.add(BigInteger.valueOf(path));
        return list;
    }
    
    public long getOperationsPerSecond() {
        long operationsPerSecond;
        long duration = getEndTimestamp() - getStartTimestamp();
        if(duration > 0) {
            operationsPerSecond = (1000 * operations) / duration;
        } else {
            operationsPerSecond = 0;
        }
        return operationsPerSecond;
    }

    // Not really MIPS - but millions of iterations per second
    public long getMIPS() {
        return getOperationsPerSecond() / 1000000;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (id != null ? id.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof UnitOfWork)) {
            return false;
        }
        UnitOfWork other = (UnitOfWork) object;
        if ((this.id == null && other.id != null) || (this.id != null && !this.id.equals(other.id))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        StringBuffer aBuffer = new StringBuffer("UnitOfWork");//getClass().getSimpleName());
        aBuffer.append("@");
        aBuffer.append(hashCode());
        aBuffer.append("( id: ");
        aBuffer.append(getId());
        aBuffer.append(" s:");
        aBuffer.append(initial);
        aBuffer.append(")");
        return aBuffer.toString();
    }    

    public void addRecord(CollatzRecord record) {
        if(null == records) {
            records = new ArrayList<CollatzRecord>();
        }
        records.add(record);        
    }

    // Composite get/set
    public BigInteger getMaxPath() {        return getMaxPathRecord().getPathLength();    }
    public BigInteger getMaxValue() {        return getMaxValueRecord().getMaximum();    }

    // Simple get/set
    public Long getId() {        return id;    }
    public void setId(Long id) {        this.id = id;    }
    public Processor getProcessor() {        return processor;    }
    public void setProcessor(ActiveProcessor processor) {        this.processor = processor;    }
    public BigInteger getInitial() {        return initial;    }
    public void setInitial(BigInteger initial) {        this.initial = initial;    }
    public BigInteger getExtent() {        return extent;    }
    public void setExtent(BigInteger extent) {        this.extent = extent;    }
    public CollatzRecord getKnownMax() {        return knownMax;    }
    public void setKnownMax(CollatzRecord knownMax) {        this.knownMax = knownMax;    }
    public CollatzRecord getKnownPath() {        return knownPath;    }
    public void setKnownPath(CollatzRecord knownPath) {        this.knownPath = knownPath;    }
    public Long getStartTimestamp() {        return startTimestamp;    }
    public void setStartTimestamp(Long startTimestamp) {        this.startTimestamp = startTimestamp;    }
    public Long getEndTimestamp() {        return endTimestamp;    }
    public void setEndTimestamp(Long endTimestamp) {        this.endTimestamp = endTimestamp;    }
    public Integer getRetries() {        return retries;    }
    public void setRetries(Integer retries) {        this.retries = retries;    }
    public Long getVersion() {        return version;    }
    public void setVersion(Long version) {        this.version = version;    }
    public List<CollatzRecord> getRecords() {        return records;    }
    public void setRecords(List<CollatzRecord> records) {        this.records = records;    }
    public long getOperations() {        return operations;    }
    public void setOperations(long operations) {        this.operations = operations;    }
    public CollatzRecord getMaxPathRecord() {        return maxPathRecord;    }
    public void setMaxPathRecord(CollatzRecord maxPathRecord) {        this.maxPathRecord = maxPathRecord;    }
    public CollatzRecord getMaxValueRecord() {        return maxValueRecord;    }
    public void setMaxValueRecord(CollatzRecord maxValueRecord) {        this.maxValueRecord = maxValueRecord;    }    
}
