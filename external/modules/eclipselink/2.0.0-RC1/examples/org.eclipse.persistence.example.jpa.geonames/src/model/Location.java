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
 *     dclarke - initial GeoNames JPA example
 ******************************************************************************/
package model;

import java.util.Calendar;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

/**
 * The Location entity represents a geographic location that a user was or will be for a specific time period.
 * 
 * @author djclarke
 * @since EclipseLink 1.0
 */
@Entity
@Table(name = "GEO_LOCATION")
public class Location {

	@Id
	@GeneratedValue()
	private long id;

	@ManyToOne 
	@JoinColumn(name = "USER_ID")
	private User user;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(name = "START_TIME")
	private Calendar start;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(name = "END_TIME")
	private Calendar end;

	public User getUser() {
		return user;
	}

	public void setUser(User user) {
		this.user = user;
	}

	public Calendar getStart() {
		return start;
	}

	public void setStart(Calendar start) {
		this.start = start;
	}

	public Calendar getEnd() {
		return end;
	}

	public void setEnd(Calendar end) {
		this.end = end;
	}

	public long getId() {
		return id;
	}

}
