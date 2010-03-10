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

import java.util.Locale;

/**
 * A CountryLanguage represents a language spoken in a particular country. Each
 * Country has a collection of these ordered by the number of speakers of the
 * language. This class is used to maintain the list and its order linking to
 * Language objects and Locale objects as appropriate based on the code values
 * provided in the GeoNames Country Info dump file.
 * 
 * @author dclarke
 * @since EclipseLink 1.1
 */
public class CountryLanguage {
    private String countryCode;

    /** Locale code as provided by the GeoNames data */
    private String code;

    private Country country;

    /** The index is maintained to ensure the order is not lost */
    private int index = 0;

    private Language language;

    /** Locale value looked up based on the GeoNames code */
    private Locale locale;

    public CountryLanguage() {
    }

    public CountryLanguage(Country country, String code, Language language) {
        setCountry(country);
        setCode(code);
        setLanguage(language);
    }

    public String getCountryCode() {
        return this.countryCode;
    }

    public Country getCountry() {
        return this.country;
    }

    protected void setCountry(Country country) {
        this.country = country;
        this.countryCode = country.getCode();
    }

    public String getCode() {
        return this.code;
    }

    public void setCode(String localeCode) {
        this.code = localeCode;
        this.locale = null;
    }

    public int getIndex() {
        return this.index;
    }

    protected void setIndex(int index) {
        this.index = index;
    }

    public Language getLanguage() {
        return this.language;
    }

    public void setLanguage(Language language) {
        this.language = language;
    }

    public Locale getLocale() {
        if (this.locale == null) {
            if (getCode().length() <= 3) {
                return new Locale(getCode());
            }
            if (getCode().length() == 5 && (getCode().charAt(2) == '-' || getCode().charAt(2) == '_')) {
                return new Locale(getCode().substring(0, 2), getCode().substring(3));
            }
        }
        return this.locale;
    }

    public String toString() {
        return "CountryLanguage(" + getCountryCode() + ": " + getCode() + ")";
    }

    public static class ID {
        private String countryCode;
        private String code;

        protected ID() {
        }

        public ID(String countryCode, String code) {
            this.countryCode = countryCode;
            this.code = code;
        }

        public String getCountryCode() {
            return this.countryCode;
        }

        public String getCode() {
            return this.code;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj != null & obj instanceof ID) {
                ID other = (ID) obj;
                return getCountryCode().equals(other.getCountryCode()) && getCode().equals(other.getCode());
            }
            return false;
        }

        @Override
        public int hashCode() {
            return getCountryCode().hashCode() + getCode().hashCode();
        }

    }

}
