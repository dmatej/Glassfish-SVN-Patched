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
 * 		dclarke - initial JPA Employee example using XML (bug 217884)
 ******************************************************************************/
package model;

import java.io.Serializable;
import java.util.Calendar;

/**
 * Represents the period of time an employee has worked for the company. A null
 * endDate indicates that the employee is current.
 * 
 * @author djclarke
 */
public class EmploymentPeriod implements Serializable {
	private Calendar startDate;
	private Calendar endDate;

	public Calendar getStartDate() {
		return startDate;
	}

	public void setStartDate(Calendar startDate) {
		this.startDate = startDate;
	}

	public void setStartDate(int year, int month, int date) {
		if (this.startDate == null) {
			setStartDate(Calendar.getInstance());
		}
		getStartDate().set(year, month, date);
	}

	public Calendar getEndDate() {
		return endDate;
	}

	public void setEndDate(Calendar endDate) {
		this.endDate = endDate;
	}

	public void setEndDate(int year, int month, int date) {
		if (this.endDate == null) {
			setEndDate(Calendar.getInstance());
		}
		getEndDate().set(year, month, date);
	}
}
