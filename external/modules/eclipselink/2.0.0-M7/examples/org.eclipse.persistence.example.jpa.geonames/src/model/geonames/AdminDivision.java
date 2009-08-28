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
 *     dclarke - initial GeoNames EclipseLink JPA example
 ******************************************************************************/
package model.geonames;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.*;

/**
 * Administrative Division of a country. This entity class is part of the GeoNames
 * EclipseLink JPA example. For more information on the example
 * @see {link}
 * 
 * Data from: {link}http://download.geonames.org/export/dump/countryInfo.txt{link}
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
@Entity
@Table(name = "GEO_ADMIN")
@IdClass(AdminDivision.ID.class)
public class AdminDivision {

	@Id
	@Column(name = "CC", insertable = false, updatable = false)
	private String countryCode;

	@Id
	private String id;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "CC")
	private Country country;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumns( {
			@JoinColumn(name = "CC", referencedColumnName = "CC", insertable = false, updatable = false),
			@JoinColumn(name = "PARENT_ID", referencedColumnName = "ID") })
	private AdminDivision parent;
	
	@OneToMany(mappedBy="parent")
	private List<AdminDivision> subdivisions;

	private String name;

	@Column(name = "ASCII_NAME")
	private String asciiName;

	@Column(name = "GEO_NAME_ID")
	private int geoNameId;

	public AdminDivision() {
		this.subdivisions = new ArrayList<AdminDivision>();
	}
	
	public AdminDivision(String countryCode, String id, String name, String asciiName, int geoNameId) {
		this();
		this.countryCode = countryCode;
		this.id = id;
		this.name = name;
		this.asciiName = asciiName;
		this.geoNameId = geoNameId;
		
		if (id == null || id.length() == 0) {
			this.id = countryCode;
		}
	}
	public String getCountryCode() {
		return this.countryCode;
	}

	public String getId() {
		return this.id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public Country getCountry() {
		return this.country;
	}

	public void setCountry(Country country) {
		this.country = country;
		this.countryCode = country.getCode();
	}

	public AdminDivision getParent() {
		return this.parent;
	}

	public void setParent(AdminDivision parent) {
		this.parent = parent;
	}

	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getAsciiName() {
		return this.asciiName;
	}

	public void setAsciiName(String asciiName) {
		this.asciiName = asciiName;
	}

	public int getGeoNameId() {
		return this.geoNameId;
	}

	public void setGeoNameId(int geoNameId) {
		this.geoNameId = geoNameId;
	}

	public String toString() {
		return "AdminDivision(" + getCountryCode() + ", " + getId() + ", " + getName() + ")";
	}

	public List<AdminDivision> getSubdivisions() {
		return this.subdivisions;
	}

	public void setSubdivisions(List<AdminDivision> subdivisions) {
		this.subdivisions = subdivisions;
	}

	public static class ID {
		private String countryCode;
		private String id;

		public String getCountryCode() {
			return this.countryCode;
		}

		public String getId() {
			return this.id;
		}

		public ID() {

		}

		public ID(String countryCode, String id) {
			this.countryCode = countryCode;
			this.id = id;
		}

		@Override
		public boolean equals(Object obj) {
			if (obj != null && obj.getClass() == ID.class) {
				ID otherId = (ID) obj;
				return getId().equals(otherId.getId()) && getCountryCode().equals(otherId.getCountryCode());
			}
			return false;
		}

		@Override
		public int hashCode() {
			return getId().hashCode() + getCountryCode().hashCode();
		}		
	}

}
