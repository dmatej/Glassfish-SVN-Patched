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
package utils.load;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.EntityManager;

import model.*;
import model.geonames.Continent;
import model.geonames.Country;
import model.geonames.CountryLanguage;
import model.geonames.Language;

/**
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
public class CountryLoader {

	public List<Country> load(EntityManager em, LanguageLoader languages) {
		TabSeparatedRowReader reader = new TabSeparatedRowReader(
				"countryInfo.txt");
		List<Country> countries = new ArrayList<Country>();

		// Skip the header and comments
		reader.skipLines(38);

		while (reader.ready()) {
			List<String> tokens = reader.readLine();

			String code = tokens.get(0);
			String isoAlpha3 = tokens.get(1);
			int isoNumeric = Integer.valueOf(tokens.get(2));
			String fips = tokens.get(3);
			String name = tokens.get(4);
			String capital = tokens.get(5);
			double area = 0d;
			if (tokens.get(6) != null && tokens.get(6).indexOf('E') < 0) {
				area = Double.valueOf(tokens.get(6));
			}
			int population = Integer.valueOf(tokens.get(7));

			String continentCode = tokens.get(8);
			Continent continent = null;
			if (em != null) {
				continent = em.find(Continent.class, continentCode);
			}

			String currency = tokens.get(10);
			int geoNameId = Integer.valueOf(tokens.get(11));

			Country country = new Country(code, isoAlpha3, isoNumeric, fips,
					name, capital, population, continent, area, currency,
					geoNameId);

			addLanguages(country, tokens.get(9), languages);

			if (em != null) {
				em.persist(country);
			}

			countries.add(country);
		}
		reader.close();

		return countries;
	}

	private static void addLanguages(Country country, String localeCodes,
			LanguageLoader languages) {
		if (localeCodes != null && localeCodes.length() > 0) {
			String[] localeValues = localeCodes.split(",");

			for (int index = 0; index < localeValues.length; index++) {
				addLanguage(country, localeValues[index], languages);
			}
		}
	}

	private static void addLanguage(Country country, String localeCode,
			LanguageLoader languages) {
		// Handle special case of Brazillian Portugese
		if (localeCode.equals("pt-BRR")) {
			localeCode = "pt-BR";
		}
		
		// Ensure there is not already a CountryLanguage with this code
		for (CountryLanguage cl: country.getLanguages()) {
			if (cl.getCode().equalsIgnoreCase(localeCode)) {
				return;
			}
		}

		Language language = null;

		language = languages.getIso639_1_Languages().get(
				localeCode.substring(0, 2));
		if (language == null) {
			language = languages.getIso639_3_Languages().get(localeCode);
		}
		if (language == null) {
			System.err.println("Could not find Language for: " + localeCode
					+ " for: " + country);
		}

		CountryLanguage countryLang = new CountryLanguage(country, localeCode,
				language);

		country.addLanguage(countryLang);
	}
}
