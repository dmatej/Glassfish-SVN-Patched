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

import javax.ejb.Local;

import org.eclipse.persistence.example.distributed.collatz.model.ActiveProcessor;
import org.eclipse.persistence.example.distributed.collatz.model.UnitOfWork;

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
@Local
public interface CollatzFacadeLocal {
    public ActiveProcessor registerProcessor(String identifier, int threads);
    public void postUnitOfWork(UnitOfWork uow, boolean retry);
    public UnitOfWork requestUnitOfWork(String identifier, int threads);
    // Management API
    public String getCurrentNumberDelimited();    
    public BigInteger getCurrentNumber();
    public void setCurrentNumber(BigInteger number);
    public String getPartitionLengthDelimited();
    public BigInteger getPartitionLength();
    public void setPartitionLength(BigInteger partition);
    // JSF Integration
    public int getMips();
    public String getMipsDelimited();
    
    public long getWorkUnits();
    public String getMaxPathDelimited();
    public BigInteger getMaxPath();
    public String getMaxValueDelimited();
    public BigInteger getMaxValue();
    public int getNumberProcessors();
}
