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
package model.geonames;

import javax.persistence.*;

/**
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
@Entity
@Table(name = "GEO_TIMEZZONE")
public class TimeZone {

	@Id
	private String name;

	/** GMT offset 1. Jan 2008 */
	private double offset;

	/** DST offset 1. Jul 2008 */
	private double dstOffset;

	public TimeZone() {
	}

	public TimeZone(String name, double offset, double dstOffset) {
		this.name = name;
		this.offset = offset;
		this.dstOffset = dstOffset;
	}

	public String getName() {
		return this.name;
	}

	public double getOffset() {
		return this.offset;
	}

	public double getDSTOffset() {
		return this.dstOffset;
	}

	public String toString() {
		return "TimeZone(" + getName() + ", " + getOffset() + ", "
				+ getDSTOffset() + ")";
	}
}
