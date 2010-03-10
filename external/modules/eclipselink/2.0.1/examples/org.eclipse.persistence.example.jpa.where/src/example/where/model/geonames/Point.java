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
package example.where.model.geonames;

import java.util.Calendar;

/**
 * 
 * 
 * @author dclarke
 * @since EclipseLink 1.1
 */
public class Point {
    private int id;
    private String name;
    private String asciiName;
    private String[] alternateNames;
    private double latitude;
    private double longitude;
    /**
     * feature class : see http://www.geonames.org/export/codes.html, char(1)
     * feature code : see http://www.geonames.org/export/codes.html, varchar(10)
     * 
     */
    private Feature feature;

    /**
     * ISO-3166 2-letter country code, 2 characters
     */
    private Country country;

    /**
     * cc2: alternate country codes, comma separated, ISO-3166 2-letter country
     * code, 60 characters
     */
    private String[] alternateCountryCodes;

    /**
     * admin1 code : fipscode (subject to change to iso code), isocode for the
     * us and ch, see file admin1Codes.txt for display names of this code;
     * varchar(20)
     */
    private AdminDivision adminDivision;

    private int population;

    /** Elevation in meters */
    private Integer elevation;

    /**
     * gtopo30: average elevation of 30'x30' (ca 900mx900m) area in meters,
     * integer
     */
    private int gtopo30;

    private TimeZone timeZone;

    /** modification date : date of last modification in yyyy-MM-dd format */
    private Calendar modificationDate;

    public Point() {

    }

    public Point(int geoNameId, String name, String asciiName, String[] alternateNames, double latitude, double longitude, Feature feature, Country country, String[] alternateCCs,
            AdminDivision adminDivision, int population, Integer elevation, int gtopo30, TimeZone tz, Calendar modDate) {
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
        this.adminDivision = adminDivision;
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

    public AdminDivision getAdminDivision() {
        return this.adminDivision;
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
        return "Point(" + getId() + ", " + getAsciiName() + ", " + getCountry() + ")";
    }
}
