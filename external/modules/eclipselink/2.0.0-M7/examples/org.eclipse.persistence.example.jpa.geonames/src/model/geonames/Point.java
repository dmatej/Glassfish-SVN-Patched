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

import java.util.Calendar;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.JoinColumns;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.eclipse.persistence.annotations.Convert;
import org.eclipse.persistence.annotations.Converter;
import org.eclipse.persistence.annotations.JoinFetch;
import org.eclipse.persistence.annotations.JoinFetchType;

import persistence.StringArrayConverter;

/**
 * 
 * The main 'geoname' table has the following fields :
 * --------------------------------------------------- geonameid : integer id of
 * record in geonames database name : name of geographical point (utf8)
 * varchar(200) asciiname : name of geographical point in plain ascii
 * characters, varchar(200) alternatenames : alternatenames, comma separated
 * varchar(4000) latitude : latitude in decimal degrees (wgs84) longitude :
 * longitude in decimal degrees (wgs84) feature class : see
 * http://www.geonames.org/export/codes.html, char(1) feature code : see
 * http://www.geonames.org/export/codes.html, varchar(10) country code :
 * ISO-3166 2-letter country code, 2 characters cc2 : alternate country codes,
 * comma separated, ISO-3166 2-letter country code, 60 characters admin1 code :
 * fipscode (subject to change to iso code), isocode for the us and ch, see file
 * admin1Codes.txt for display names of this code; varchar(20) admin2 code :
 * code for the second administrative division, a county in the US, see file
 * admin2Codes.txt; varchar(80) admin3 code : code for third level
 * administrative division, varchar(20) admin4 code : code for fourth level
 * administrative division, varchar(20) population : integer elevation : in
 * meters, integer gtopo30 : average elevation of 30'x30' (ca 900mx900m) area in
 * meters, integer timezone : the timezone id (see file timeZone.txt)
 * modification date : date of last modification in yyyy-MM-dd format
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
@Entity
@Table(name = "GEO_POINT")
@Converter(name = "string-array", converterClass = StringArrayConverter.class)
public class Point {

	@Id
	private int id;

	private String name;

	private String asciiName;

	@Convert("string-array")
	@Column(name = "ALT_NAMES")
	private String[] alternateNames;

	private double latitude;

	private double longitude;

	/**
	 * feature class : see http://www.geonames.org/export/codes.html, char(1)
	 * feature code : see http://www.geonames.org/export/codes.html, varchar(10)
	 * 
	 */
	@ManyToOne
	@JoinColumns( {
			@JoinColumn(name = "FEATURE_CLASS", referencedColumnName = "FEATURE_CLASS"),
			@JoinColumn(name = "FEATURE_CODE", referencedColumnName = "CODE") })
	@JoinFetch(JoinFetchType.OUTER)
	private Feature feature;

	/**
	 * ISO-3166 2-letter country code, 2 characters
	 */
	@JoinColumn(name = "CC")
	@ManyToOne(fetch = FetchType.LAZY)
	private Country country;

	/**
	 * cc2: alternate country codes, comma separated, ISO-3166 2-letter country
	 * code, 60 characters
	 */
	@Convert("string-array")
	@Column(name = "ALT_CCS")
	private String[] alternateCountryCodes;

	/**
	 * admin1 code : fipscode (subject to change to iso code), isocode for the
	 * us and ch, see file admin1Codes.txt for display names of this code;
	 * varchar(20)
	 */
	@Column(name = "ADMIN1")
	private String admin1Code;

	/** Code for the second administrative division, a county in the US */
	@Column(name = "ADMIN2")
	private String adminCode2;

	/** admin3 code: code for third level administrative division, varchar(20) */
	@Column(name = "ADMIN3")
	private String adminCode3;

	/** Code for fourth level administrative division, varchar(20) */
	@Column(name = "ADMIN4")
	private String adminCode4;

	private int population;

	/** Elevation in meters */
	private Integer elevation;

	/**
	 * gtopo30: average elevation of 30'x30' (ca 900mx900m) area in meters,
	 * integer
	 */
	private int gtopo30;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "TZ")
	private TimeZone timeZone;

	/** modification date : date of last modification in yyyy-MM-dd format */
	@Column(name = "MOD_DATE")
	@Temporal(TemporalType.DATE)
	private Calendar modificationDate;

	public Point() {

	}

	public Point(int geoNameId, String name, String asciiName,
			String[] alternateNames, double latitude, double longitude,
			Feature feature, Country country, String[] alternateCCs,
			String adminCode1, String adminCode2, String adminCode3,
			String adminCode4, int population, Integer elevation, int gtopo30,
			TimeZone tz, Calendar modDate) {
		this();
		this.id = geoNameId;
		this.name = name;
		this.asciiName = asciiName;
		this.alternateNames = alternateNames;
		this.latitude = latitude;
		this.longitude = longitude;
		this.feature = feature;
		this.country = country;
		this.alternateCountryCodes = alternateCCs;
		this.admin1Code = adminCode1;
		this.adminCode2 = adminCode2;
		this.adminCode3 = adminCode3;
		this.adminCode4 = adminCode4;
		this.population = population;
		this.elevation = elevation;
		this.gtopo30 = gtopo30;
		this.timeZone = tz;
		this.modificationDate = modDate;
	}

	public int getId() {
		return this.id;
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

	public String[] getAlternateNames() {
		return this.alternateNames;
	}

	public void setAlternateNames(String[] alternateNames) {
		this.alternateNames = alternateNames;
	}

	public double getLatitude() {
		return this.latitude;
	}

	public void setLatitude(double latitude) {
		this.latitude = latitude;
	}

	public double getLongitude() {
		return this.longitude;
	}

	public void setLongitude(double longitude) {
		this.longitude = longitude;
	}

	public Feature getFeature() {
		return this.feature;
	}

	public void setFeature(Feature feature) {
		this.feature = feature;
	}

	public Country getCountry() {
		return this.country;
	}

	public void setCountry(Country country) {
		this.country = country;
	}

	public String[] getAlternateCountryCodes() {
		return this.alternateCountryCodes;
	}

	public void setAlternateCountryCodes(String[] alternateCOuntryCodes) {
		this.alternateCountryCodes = alternateCOuntryCodes;
	}

	public String getAdmin1Code() {
		return this.admin1Code;
	}

	public void setAdmin1Code(String admin1Code) {
		this.admin1Code = admin1Code;
	}

	public String getAdminCode2() {
		return this.adminCode2;
	}

	public void setAdminCode2(String adminCode2) {
		this.adminCode2 = adminCode2;
	}

	public String getAdminCode3() {
		return this.adminCode3;
	}

	public void setAdminCode3(String adminCode3) {
		this.adminCode3 = adminCode3;
	}

	public String getAdminCode4() {
		return this.adminCode4;
	}

	public void setAdminCode4(String adminCode4) {
		this.adminCode4 = adminCode4;
	}

	public int getPopulation() {
		return this.population;
	}

	public void setPopulation(int population) {
		this.population = population;
	}

	public Integer getElevation() {
		return this.elevation;
	}

	public void setElevation(Integer elevation) {
		this.elevation = elevation;
	}

	public int getGtopo30() {
		return this.gtopo30;
	}

	public void setGtopo30(int gtopo30) {
		this.gtopo30 = gtopo30;
	}

	public TimeZone getTimeZone() {
		return this.timeZone;
	}

	public void setTimeZone(TimeZone timeZone) {
		this.timeZone = timeZone;
	}

	public Calendar getModificationDate() {
		return this.modificationDate;
	}

	public void setModificationDate(Calendar modificationDate) {
		this.modificationDate = modificationDate;
	}

	public String toString() {
		return "Point(" + getId() + ", " + getAsciiName() + ", " + getCountry()
				+ ")";
	}
}
