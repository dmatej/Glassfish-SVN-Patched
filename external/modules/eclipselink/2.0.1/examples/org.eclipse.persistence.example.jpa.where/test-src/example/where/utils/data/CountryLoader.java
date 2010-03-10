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
package example.where.utils.data;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.EntityManager;

import example.where.model.geonames.*;

/**
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
public class CountryLoader {

	protected static final String FILE_NAME = "countryInfo.txt";

	public static void main(String[] args) throws Exception {
		TabSeparatedRowReader reader = new TabSeparatedRowReader(FILE_NAME);

		// Skip the header and comments
		reader.skipLinesUntil("#ISO");

		while (reader.ready()) {
			CountryInfo country = new CountryInfo(reader.readLine());
			country.printDetails(System.out);
		}
	}

	public List<Country> load(EntityManager em, LanguageLoader languages) {
		TabSeparatedRowReader reader = new TabSeparatedRowReader(FILE_NAME);
		List<Country> countries = new ArrayList<Country>();

		// Skip the header and comments
		reader.skipLinesUntil("#ISO");

		while (reader.ready()) {
			CountryInfo countryInfo = new CountryInfo(reader.readLine());
			System.out.println("Loading> " + countryInfo);

			Country country = countryInfo.buildCountry(em);

			if (country != null) {
				addLanguages(country, countryInfo.getStrings(CountryInfo.Languages), languages);

				if (em != null) {
					em.persist(country);
				}
				countries.add(country);
			}
		}
		reader.close();

		return countries;
	}

	private static void addLanguages(Country country, String[] localeCodes, LanguageLoader languages) {
		for (int index = 0; localeCodes != null && index < localeCodes.length; index++) {
			addLanguage(country, localeCodes[index], languages);
		}
	}

	private static void addLanguage(Country country, String localeCode, LanguageLoader languages) {
		// Handle special case of Brazillian Portugese
		if (localeCode.equals("pt-BRR")) {
			localeCode = "pt-BR";
		}

		// Ensure there is not already a CountryLanguage with this code
		for (CountryLanguage cl : country.getLanguages()) {
			if (cl.getCode().equalsIgnoreCase(localeCode)) {
				return;
			}
		}

		Language language = null;

		language = languages.getIso639_1_Languages().get(localeCode.substring(0, 2));
		if (language == null) {
			language = languages.getIso639_3_Languages().get(localeCode);
		}
		if (language == null) {
			System.err.println("Could not find Language for: " + localeCode + " for: " + country);
		}

		CountryLanguage countryLang = new CountryLanguage(country, localeCode, language);

		country.addLanguage(countryLang);
	}

	/**
	 * Wrapper for an individual data row from the file.
	 */
	private static class CountryInfo {

		private static final int ISO = 0;
		private static final int ISO3 = 1;
		private static final int ISO_Numeric = 2;
		private static final int fips = 3;
		private static final int Country = 4;
		private static final int Capital = 5;
		private static final int Area = 6;
		private static final int Population = 7;
		private static final int Continent = 8;
		private static final int tld = 9;
		private static final int CurrencyCode = 10;
		private static final int CurrencyName = 11;
		private static final int Phone = 12;
		private static final int Postal_Code_Format = 13;
		private static final int Postal_Code_Regex = 14;
		private static final int Languages = 15;
		private static final int geonameid = 16;
		private static final int neighbours = 17;
		private static final int EquivalentFipsCode = 18;

		private List<String> values;

		private CountryInfo(List<String> values) {
			this.values = values;
		}

		public String getString(int index) {
			if (this.values == null || index >= this.values.size()) {
				return null;
			}
			return this.values.get(index);
		}

		public Integer getInteger(int index) {
			if (this.values == null || index >= this.values.size()) {
				return null;
			}
			return Integer.valueOf(this.values.get(index));
		}

		public String[] getStrings(int index) {
			if (this.values == null || index >= this.values.size() || this.values.get(index) == null || this.values.get(index).length() == 0) {
				return new String[0];
			}

			return this.values.get(index).split(",");
		}

		public Country buildCountry(EntityManager em) {

			String areaValue = getString(CountryInfo.Area);
			double area = 0d;
			if (areaValue != null && areaValue.indexOf('E') < 0) {
				area = Double.valueOf(areaValue);
			}
			String continentCode = getString(CountryInfo.Continent);

			if (continentCode == null) {
				System.err.println("Invalid continent code: " + continentCode + " for: " + toString());
				return null;
			}
			Continent continent = null;
			if (em != null) {
				continent = em.find(Continent.class, continentCode);
			}

			return new Country(getString(ISO), getString(ISO3), getInteger(ISO_Numeric), getString(CountryInfo.fips), getString(Country), getString(Capital), getInteger(Population), continent, area,
					getString(CurrencyCode), getInteger(geonameid));
		}

		public String toString() {
			return "CountryInfo(" + getString(ISO) + ", " + getString(Country) + ")";
		}

		public void printDetails(PrintStream out) {
			out.println(toString());

			out.println("\t ISO3> " + getString(ISO3));
			out.println("\t ISO_Numeric> " + getString(ISO_Numeric));
			out.println("\t fips> " + getString(fips));
			out.println("\t Capital> " + getString(Capital));
			out.println("\t Area> " + getString(Area));
			out.println("\t Population> " + getString(Population));
			out.println("\t Continent> " + getString(Continent));
			out.println("\t tld> " + getString(tld));
			out.println("\t CurrencyCode> " + getString(CurrencyCode));
			out.println("\t CurrencyName> " + getString(CurrencyName));
			out.println("\t Phone> " + getString(Phone));
			out.println("\t Postal_Code_Format> " + getString(Postal_Code_Format));
			out.println("\t Postal_Code_Regex> " + getString(Postal_Code_Regex));
			out.println("\t Languages> " + getString(Languages));
			out.println("\t geonameid> " + getString(geonameid));
			out.println("\t neighbours> " + getString(neighbours));
			out.println("\t EquivalentFipsCode> " + getString(EquivalentFipsCode));
		}
	}
}
