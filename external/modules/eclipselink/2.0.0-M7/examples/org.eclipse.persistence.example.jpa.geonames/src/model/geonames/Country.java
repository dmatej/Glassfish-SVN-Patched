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

import org.eclipse.persistence.annotations.Converter;

import persistence.LocalesConverter;

/**
 * 
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
@Entity
@Table(name = "GEO_COUNTRY")
@Converter(name = "locales", converterClass = LocalesConverter.class)
public class Country {
	/** ISO 2 letter Country Code */
	@Id
	@Column(name = "CC")
	private String code;

	private String isoAlpha3;
	private int isoNumeric;

	/** Some 2 letter code */
	private String fips;
	private String name;

	private String capital;

	private int population;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "CONTINENT_CODE")
	private Continent continent;

	/** Area in square km */
	private double area;

	/** the languages spoken in a country ordered by the number of speakers */
	@OneToMany(mappedBy="country", cascade=CascadeType.ALL)
	private List<CountryLanguage> languages;

	private String currency;

	private int geoNameId;

	public Country() {
		this.languages = new ArrayList<CountryLanguage>();
	}

	public Country(String isoCode, String isoAlpha3, int isoNumeric,
			String fips, String name, String capital, int population,
			Continent continent, double area, String currency, int geoNameId) {
		this();
		this.code = isoCode;
		this.isoAlpha3 = isoAlpha3;
		this.isoNumeric = isoNumeric;
		this.fips = fips;
		this.name = name;
		this.capital = capital;
		this.population = population;
		this.continent = continent;
		this.area = area;
		this.currency = currency;
		this.geoNameId = geoNameId;
	}

	public String getCode() {
		return this.code;
	}

	public void setCode(String code) {
		this.code = code;
	}

	public String getIsoAlpha3() {
		return this.isoAlpha3;
	}

	public void setIsoAlpha3(String isoAlpha3) {
		this.isoAlpha3 = isoAlpha3;
	}

	public int getIsoNumeric() {
		return this.isoNumeric;
	}

	public void setIsoNumeric(int isoNumeric) {
		this.isoNumeric = isoNumeric;
	}

	public String getFips() {
		return this.fips;
	}

	public void setFips(String fips) {
		this.fips = fips;
	}

	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getCapital() {
		return this.capital;
	}

	public void setCapital(String capital) {
		this.capital = capital;
	}

	public int getPopulation() {
		return this.population;
	}

	public void setPopulation(int population) {
		this.population = population;
	}

	public Continent getContinent() {
		return this.continent;
	}

	public void setContinent(Continent continent) {
		this.continent = continent;
	}

	public double getArea() {
		return this.area;
	}

	public void setArea(double area) {
		this.area = area;
	}

	public List<CountryLanguage> getLanguages() {
		return this.languages;
	}

	public void addLanguage(CountryLanguage language) {
		if (!getLanguages().contains(language)) {
			getLanguages().add(language);
		}
		language.setIndex(getLanguages().indexOf(language));
	}

	public String getCurrency() {
		return this.currency;
	}

	public void setCurrency(String currency) {
		this.currency = currency;
	}

	public int getGeoNameId() {
		return this.geoNameId;
	}

	public void setGeoNameId(int geoNameId) {
		this.geoNameId = geoNameId;
	}

	public String toString() {
		return "Country(" + getCode() + ", " + getName() + ")";
	}
}
