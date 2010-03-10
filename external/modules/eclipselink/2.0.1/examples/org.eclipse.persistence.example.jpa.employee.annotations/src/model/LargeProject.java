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
 *              dclarke - initial JPA Employee example using XML (bug 217884)
 *              mbraeuer - annotated version
 ******************************************************************************/
package model;

import static javax.persistence.TemporalType.TIMESTAMP;

import java.util.Calendar;

import javax.persistence.*;

/**
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
@Entity
@Table(name = "LPROJECT")
@DiscriminatorValue("L")
public class LargeProject extends Project {
	@Basic
	private double budget;
	@Basic
	@Temporal(TIMESTAMP)
	private Calendar milestone = Calendar.getInstance();

	public LargeProject() {
		super();
	}

	public double getBudget() {
		return this.budget;
	}

	public void setBudget(double budget) {
		this.budget = budget;
	}

	public Calendar getMilestone() {
		return milestone;
	}

	public void setMilestone(Calendar milestone) {
		this.milestone = milestone;
	}
}
