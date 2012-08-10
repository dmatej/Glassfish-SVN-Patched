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
package org.eclipse.persistence.example.distributed.collatz.business;

import java.math.BigInteger;
import java.util.Date;
import java.util.List;

import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.NoResultException;
import javax.persistence.NonUniqueResultException;
import javax.persistence.OptimisticLockException;
import javax.persistence.PersistenceContext;
import javax.persistence.PersistenceContextType;
import javax.persistence.Query;

import org.eclipse.persistence.example.distributed.collatz.model.ActiveProcessor;
import org.eclipse.persistence.example.distributed.collatz.model.CollatzRecord;
import org.eclipse.persistence.example.distributed.collatz.model.ComputeGrid;
import org.eclipse.persistence.example.distributed.collatz.model.UnitOfWork;

/**
 * Session Bean implementation class CollatzFacade<br>
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
@Stateless(mappedName = "ejb/CollatzFacade")
public class CollatzFacade implements CollatzFacadeRemote, CollatzFacadeLocal {

    @PersistenceContext(unitName = "CollatzGF-ejbPU", type=PersistenceContextType.TRANSACTION)
    private EntityManager entityManager;

    // Some special numbers
    /** The following Collatz number at 61 bits has a maximum path of 64,024667,322193,133530,165877,294264,738020 at 125 bits */
    public static final BigInteger COLLATZ88 = BigInteger.valueOf(1980976057694848447L);     

    // Hibernate supports 19bit numbers on Derby by default
    // EclipseLink supports 32bit numbers on Derby by default
    // Override both by declaring a @TypeConverter
    public static final long INITIAL_SEARCH_INTERVAL = 1 << 22;
    //public static final BigInteger INITIAL_START = BigInteger.ONE.shiftLeft(124);
    public static final BigInteger INITIAL_START = BigInteger.valueOf(27);   
    
    /**
     * 
     */
    public UnitOfWork requestUnitOfWork(String identifier, int threads) {
        // ask collatz for the next unit of work range
        UnitOfWork uow = null;
        ActiveProcessor processor = null;
        try {
            Query queryProcessor = entityManager.createQuery("select object(p) from ActiveProcessor p where p.identifier= :processor'");
            queryProcessor.setParameter("processor", identifier);
            try {
                processor = (ActiveProcessor)queryProcessor.getSingleResult();
            } catch (NoResultException nre) {
                System.out.println("_collatz: " + System.currentTimeMillis() + ": server was redeployed mid-session, or new processor is registering: " + identifier);
            }

            // If the processor record does not yet exist - create one            
            if(null == processor) {
                processor = new ActiveProcessor();
                processor.setIdentifier(identifier);
                processor.setThreads(threads);
                entityManager.persist(processor);
                System.out.println("_collatz: " + System.currentTimeMillis() + ": Creating new: " + processor + " for " + identifier);
            }

            // ask for the next number to search
            ComputeGrid computeGrid = getComputeGrid();
            
            // update INITIAL_START initial start if it changed - by deleting the record (to protect the database)
            // update search interval if it changed
            if(INITIAL_SEARCH_INTERVAL != computeGrid.getPartitionLength().longValue()) {
                computeGrid.setPartitionLength(BigInteger.valueOf(INITIAL_SEARCH_INTERVAL));
            }
            BigInteger nextNumber = computeGrid.getNextNumberToSearch();
            BigInteger partition = computeGrid.getPartitionLength();
            BigInteger extent = nextNumber.add(partition);
            System.out.println("_collatz: " + System.currentTimeMillis() + ": requestUnitOfWork(" + nextNumber + "-" + extent + ") for processor " + processor);
            // create a unit of work packet for the client
            uow = new UnitOfWork();
            // make the number odd
            if(!nextNumber.testBit(0)) {
                uow.setInitial(nextNumber.add(BigInteger.ONE));
            } else {
            	uow.setInitial(nextNumber);
            }
            uow.setExtent(extent);
            // TODO: max will be different for different ranges
            // TODO: KnownMax/Path are kind of redundant now that we associate a record instead of just a BigInteger with UOW and ComputeGrid
            uow.setKnownMax(computeGrid.getMaxValueRecord());
            uow.setKnownPath(computeGrid.getMaxPathRecord());
            uow.setMaxPathRecord(computeGrid.getMaxPathRecord());
            uow.setMaxValueRecord(computeGrid.getMaxValueRecord());
            uow.setProcessor(processor);
            uow.setStartTimestamp((new Date()).getTime());
            uow.setRetries(0);

            // only update the database with the new global numbers if the packet was returned
            //computeGrid.setNextNumberToSearch(extent);
            //em.persist(computeGrid)
            entityManager.persist(processor);
            entityManager.persist(uow);
        //} catch (OptimisticLockException ole) { // should not get this for different processors
        //    ole.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return uow;
    }

    
    // TODO: if packet not received - remark uow for another processor
    /**
     * Handles OptimisticLockException
     */
    // split storage from computeGrid, on ole just read
    public void postUnitOfWork(UnitOfWork uow, boolean retry) {
        // get the current maximum and path just under the initial # of this uow
        ComputeGrid computeGrid = null;
            // The first time through we want to read and compare (the same as we would if we encountered an OptimisticLockException)
            boolean hasOptimisticLockException = true;
            while (hasOptimisticLockException) {
                hasOptimisticLockException = false;
                try {
                    computeGrid = getComputeGrid();
                    // collate local and global records
                if(computeGrid.getMaxPath().compareTo(uow.getMaxPath()) < 0) {
                	StringBuffer aBuffer = new StringBuffer("_collatz: ");
                	aBuffer.append(System.currentTimeMillis());
                	aBuffer.append(": New max path : ");
                	aBuffer.append(uow.getMaxPathRecord().getInitial());
                	aBuffer.append(",");
                	aBuffer.append(uow.getMaxPathRecord().getPathLength());
                	aBuffer.append(",");
                	aBuffer.append(uow.getMaxPathRecord().getMaximum());
                	aBuffer.append(" via: ");
                	aBuffer.append(uow.getProcessor().getIdentifier());
                	aBuffer.append(" # ");
                	aBuffer.append(uow.getId());
                	aBuffer.append(" @ " );
            		aBuffer.append(uow.getMIPS());
            		aBuffer.append(" MIPS");
            		computeGrid.setMaxPathRecord(uow.getMaxPathRecord());
                }
                if(computeGrid.getMaxValue().compareTo(uow.getMaxValue()) < 0) {
                	StringBuffer aBuffer = new StringBuffer("_collatz: ");
                	aBuffer.append(System.currentTimeMillis());
                	aBuffer.append(": New max value: ");
                	aBuffer.append(uow.getMaxValueRecord().getInitial());
                	aBuffer.append(",");
                	aBuffer.append(uow.getMaxValueRecord().getPathLength());
                	aBuffer.append(",");
                	aBuffer.append(uow.getMaxValueRecord().getMaximum());
                	aBuffer.append(" via: ");
                	aBuffer.append(uow.getProcessor().getIdentifier());
                	aBuffer.append(" # ");
                	aBuffer.append(uow.getId());
                	aBuffer.append(" @ " );
            		aBuffer.append(uow.getMIPS());
            		aBuffer.append(" MIPS");
            		computeGrid.setMaxValueRecord(uow.getMaxValueRecord());
                }
                BigInteger nextNumber = computeGrid.getNextNumberToSearch();
                BigInteger partition = computeGrid.getPartitionLength();
                BigInteger extent = nextNumber.add(partition);
                // only update the database with the new global numbers if the packet was returned
                computeGrid.setNextNumberToSearch(extent);  // no persist req for cm transactions/
                // TODO: SET global duration so mips does not drop when the server is idle
                
                computeGrid.setOperations(BigInteger.valueOf(uow.getOperations()).add(computeGrid.getOperations()));
                // Perf: the database record may have been modified since it was last read.
            // filter the new record from the run
            List<CollatzRecord> records = uow.getRecords();
            if(null != records && records.size() > 0) {
                //System.out.println("_collatz: " + System.currentTimeMillis() + ": persisting " + records.size() + " CollatzRecords");
                for(CollatzRecord record : records) {
                    // need to persist max/path as well
                    entityManager.persist(record);
                }
            }
            } catch (OptimisticLockException ole ) {
            	System.out.println(ole.getMessage());
            	hasOptimisticLockException = false;
            }
        }
    }
    
    public ActiveProcessor registerProcessor(String identifier, int threads) {
        ActiveProcessor processor = null;
        // check if processor already registered
        return processor;
    }
    
    public UnitOfWork getUnitOfWork(Long id) {
        return entityManager.find(UnitOfWork.class, id);
    }

    // Management API
    public BigInteger getCurrentNumber() {
        return getComputeGrid().getNextNumberToSearch();
    }
        
    public String getCurrentNumberDelimited() {
        return getDelimitedNumber(getCurrentNumber().toString());
    }
    
    public void setCurrentNumber(BigInteger number) {
        ComputeGrid computeGrid = getComputeGrid();
        computeGrid.setNextNumberToSearch(number);
        // persist this
        entityManager.persist(computeGrid);
    }

    public String getPartitionLengthDelimited() {
        return getDelimitedNumber(getPartitionLength().toString());
    }

    public BigInteger getPartitionLength() {
        return getComputeGrid().getPartitionLength();
    }
    
    public void setPartitionLength(BigInteger partition) {
        ComputeGrid computeGrid = getComputeGrid();
        computeGrid.setPartitionLength(partition);
        // persist this
        entityManager.persist(computeGrid);
    }

    private synchronized ComputeGrid initializeComputeGrid(UnitOfWork uow) {
        ComputeGrid computeGrid = new ComputeGrid();
        computeGrid.setPartitionLength(BigInteger.valueOf(INITIAL_SEARCH_INTERVAL));
        computeGrid.setGlobalStartTimestamp((new Date()).getTime());
        computeGrid.setNextNumberToSearch(INITIAL_START);
        computeGrid.setOperations(BigInteger.ZERO);
        
        CollatzRecord milestoneRecord = new CollatzRecord();
        milestoneRecord.setIsMaxRecord(true);
        milestoneRecord.setIsPathRecord(true);
        milestoneRecord.setInitial(computeGrid.getNextNumberToSearch());
        milestoneRecord.setMaximum(BigInteger.valueOf(9232));
        milestoneRecord.setPathLength(BigInteger.valueOf(110));
        computeGrid.setMaxPathRecord(milestoneRecord);
        computeGrid.setMaxValueRecord(milestoneRecord);
        System.out.println("_collatz: " + System.currentTimeMillis() + ": First packet: " + computeGrid);
        // UnitOfWork will be persisted later in request - if we are processing a response then persist changes
        try {
            if(null != uow) {
                milestoneRecord.setUnitOfWork(uow);
                uow.setKnownMax(milestoneRecord);
                uow.setKnownPath(milestoneRecord);
                uow.setMaxPathRecord(milestoneRecord);
                uow.setMaxValueRecord(milestoneRecord);
                // TODO: set maximums on ComputeGrid entity
                entityManager.persist(uow);
            }
            entityManager.persist(milestoneRecord);
            synchronized(computeGrid) {
            	entityManager.persist(computeGrid);
            }
            // computeGrid.setPartitionLength(BigInteger.valueOf(INITIAL_SEARCH_INTERVAL));
        } catch (Exception ole) {
            System.out.println("_collatz: " + System.currentTimeMillis() + ": OptimisticLockException for: " + computeGrid);
            // merge changes and re-persist
            ComputeGrid oldComputeGrid = null;
            // JPA 2.0
            //Query query = em.createQuery("select object(c) from ComputeGrid c", ComputeGrid.class);
            // JPA 1.0
            Query query = entityManager.createQuery("select object(p) from ComputeGrid c");
            try {
                oldComputeGrid = (ComputeGrid)query.getSingleResult();
            } catch (NoResultException nre) {
                //System.out.println("_collatz: " + System.currentTimeMillis() + ": server was redeployed mid-session: ComputeGrid is null");
                return null;
            }
            // Merge results
            if(computeGrid.getMaxPath().compareTo(oldComputeGrid.getMaxPath()) > 0) {
                oldComputeGrid.setMaxPathRecord(computeGrid.getMaxPathRecord());
            }
            if(computeGrid.getMaxValue().compareTo(oldComputeGrid.getMaxValue()) > 0) {
                oldComputeGrid.setMaxValueRecord(computeGrid.getMaxValueRecord());
            }
        }
        return computeGrid;
    }

    /**
     * This method should never return a null Entity
     * @return
     */
    private synchronized ComputeGrid getComputeGrid() {        
        // We will let the persistence provider L1 cache store the entities between requests in the same session bean session. (no instance variable on this bean)
        ComputeGrid computeGrid = null;
        // JPQL Ref: http://download.oracle.com/docs/cd/E11035_01/kodo41/full/html/ejb3_langref.html
        // JPA 2.0
        //Query query = em.createQuery("select object(c) from ComputeGrid c", ComputeGrid.class);
        // JPA 1.0
        Query query = entityManager.createQuery("select object(c) from ComputeGrid c");
        try {
            computeGrid = (ComputeGrid)query.getSingleResult();
        } catch (NoResultException nre) {
            //System.out.println("_collatz: " + System.currentTimeMillis() + ": server was redeployed mid-session: computeGrid is null");
            computeGrid = initializeComputeGrid(null);
        }
        return computeGrid;
    }

    public String getMipsDelimited() {
        return getDelimitedNumber(String.valueOf(getMips()));
    }

    // JSF Integration
    public int getMips() {
    	BigInteger operations = BigInteger.ZERO;
    	int mips = 0;
    	Query query = entityManager.createQuery("select object(c) from ComputeGrid c");
    	ComputeGrid computeGrid = null;
        try {
            computeGrid = (ComputeGrid)query.getSingleResult();
            operations = computeGrid.getOperations();
            long startTime = computeGrid.getGlobalStartTimestamp();
            long duration =  System.currentTimeMillis() - startTime;
            if(duration > 0) {
                mips = (operations.multiply(BigInteger.valueOf(1000L))).divide(BigInteger.valueOf(duration)).intValue();
            }
        } catch (NonUniqueResultException nure) {
            System.out.println("_collatz: " + System.currentTimeMillis() + ": server was redeployed mid-session, or new processor is registering: unable to compute MIPS:" + nure.getMessage());
            mips = -1;            
        } catch (NoResultException nre) {
            System.out.println("_collatz: " + System.currentTimeMillis() + ": server was redeployed mid-session, or new processor is registering: ");
        }
        return mips;
    }

    public long getWorkUnits() {
    	long workUnits = -1;
        Query query = entityManager.createQuery("select object(p) from UnitOfWork p'");
        List processors = null;
        try {
            processors = query.getResultList();
            if(null != processors) {
            	workUnits = processors.size();
            }
        } catch (NoResultException nre) {
            System.out.println("_collatz: " + System.currentTimeMillis() + ": server was redeployed mid-session, or new processor is registering: ");
        }
        return workUnits;
    }
    
    public String getMaxPathDelimited() {
        return getDelimitedNumber(getMaxPath().toString());
    }    
    
    public BigInteger getMaxPath() {
    	BigInteger maxPath = BigInteger.ZERO;
    	Query query = entityManager.createQuery("select object(c) from ComputeGrid c");
    	ComputeGrid computeGrid = null;
        try {
            computeGrid = (ComputeGrid)query.getSingleResult();
            maxPath = computeGrid.getMaxPath();
        } catch (NoResultException nre) {
            System.out.println("_collatz: " + System.currentTimeMillis() + ": server was redeployed mid-session, or new processor is registering: ");
        }
        return maxPath;
    }
    
    public String getMaxValueDelimited() {
        return getDelimitedNumber(getMaxValue().toString());
    }

    /**
     * Return a comma delimited number 9323 = 9,232
     * @return
     */
    private String getDelimitedNumber(String number) {        
        // insert commas from the end every 6 digits
        StringBuffer buffer = new StringBuffer();
        short radix = 2;
        boolean skipFirst = true;
        for(int i=number.length();i > 0; i--) {
            if(radix++ > 1) {
                if(!skipFirst) {
                    buffer.append(",");
                } else {
                    skipFirst = false;
                }
                radix = 0;                
            }
            buffer.append(number.charAt(i-1));
        }
        return buffer.reverse().toString();
    }
    
    public BigInteger getMaxValue() {
    	BigInteger maxValue = BigInteger.ZERO;
    	Query query = entityManager.createQuery("select object(c) from ComputeGrid c");
    	ComputeGrid computeGrid = null;
        try {
            computeGrid = (ComputeGrid)query.getSingleResult();
            maxValue = computeGrid.getMaxValue();
        } catch (NoResultException nre) {
            System.out.println("_collatz: " + System.currentTimeMillis() + ": server was redeployed mid-session, or new processor is registering: ");
        }
        return maxValue;    	
    }
    
    public int getNumberProcessors() {
    	int numProcessors = -1;
        Query queryProcessor = entityManager.createQuery("select object(p) from ActiveProcessor p'");
        List processors = null;
        try {
            processors = queryProcessor.getResultList();
            if(null != processors) {
            	numProcessors = processors.size();
            }
        } catch (NoResultException nre) {
            System.out.println("_collatz: " + System.currentTimeMillis() + ": server was redeployed mid-session, or new processor is registering: ");
        }
        return numProcessors;
    	
    }
    
    /**
     * Default constructor - test private override. 
     */
    //private CollatzFacade() {    } // not valid for pojo spec - for variant testing only
}
