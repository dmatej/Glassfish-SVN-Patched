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
package org.eclipse.persistence.example.distributed.collatz.presentation;

import java.math.BigInteger;

import javax.ejb.EJB;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;

import org.eclipse.persistence.example.distributed.collatz.business.CollatzFacadeLocal;

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

@ManagedBean(name="monitorBean")
@SessionScoped
public class MonitorManagedBean {
    @EJB(name="ejb/CollatzFacade")
    private CollatzFacadeLocal collatzFacade;
    
    private String currentNumber;
    private String interval;
    private String mips;
    private String workUnits;
    private String maxPath;
    private String maxValue;
    private String processors;
    
    public String getCurrentNumber() {
    	currentNumber = collatzFacade.getCurrentNumber().toString();
		return currentNumber;
	}

	public void setCurrentNumber(String currentNumber) {
		this.currentNumber = currentNumber;
		collatzFacade.setCurrentNumber(BigInteger.valueOf(Long.valueOf(currentNumber).longValue()));
	}

	public String getInterval() {
		interval = collatzFacade.getPartitionLength().toString();
		return interval;
	}

	public void setInterval(String interval) {
		this.interval = interval;
	}

	public String getMips() {
		mips = String.valueOf(collatzFacade.getMips());
		return mips;
	}

	// Mips is read only
	public void setMips(String mips) {
		this.mips = mips;
	}

	// read only
	public String getWorkUnits() {
		workUnits = String.valueOf(collatzFacade.getWorkUnits());
		return workUnits;
	}

	public void setWorkUnits(String workUnits) {
		this.workUnits = workUnits;
	}

	public String getMaxPath() {
		maxPath = collatzFacade.getMaxPath().toString();
		return maxPath;
	}

	// could be read only
	public void setMaxPath(String maxPath) {
		this.maxPath = maxPath;
	}

	public String getMaxValue() {
		maxValue = collatzFacade.getMaxValue().toString();
		return maxValue;
	}

	// could be read only
	public void setMaxValue(String maxValue) {
		this.maxValue = maxValue;
	}

	public String getProcessors() {
		processors = String.valueOf(collatzFacade.getNumberProcessors());
		return processors;
	}

	// read only
	public void setProcessors(String processors) {
		this.processors = processors;
	}

	public MonitorManagedBean() {
		
    }
   
}
