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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.*;

/**
 * Continent codes :
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
@Entity
@Table(name = "GEO_CONTINENT")
public class Continent {
	@Id
	private String code;

	private String name;

	private int geoNameId;

	public Continent() {

	}

	protected Continent(String code, String name, int geoNameId) {
		this();
		this.code = code;
		this.name = name;
		this.geoNameId = geoNameId;
	}

	/**
	 * {link}http://download.geonames.org/export/dump/readme.txt{link}
	 * 
	 * @return
	 */
	public static List<Continent> getAllContinents() {
		ArrayList<Continent> continents = new ArrayList<Continent>();
		
		continents.add(new Continent("AF", "Africa", 6255146));
		continents.add(new Continent("AS", "Asia", 6255147));
		continents.add(new Continent("EU", "Europe", 6255148));
		continents.add(new Continent("NA", "North America", 6255149));
		continents.add(new Continent("OC", "Oceania", 6255151));
		continents.add(new Continent("SA", "South America", 6255150));
		continents.add(new Continent("AN", "Antarctica", 6255152));
		
		return continents;
	}

	public String getCode() {
		return this.code;
	}

	public void setCode(String code) {
		this.code = code;
	}

	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public int getGeoNameId() {
		return this.geoNameId;
	}

	public void setGeoNameId(int geoNameId) {
		this.geoNameId = geoNameId;
	}
}
