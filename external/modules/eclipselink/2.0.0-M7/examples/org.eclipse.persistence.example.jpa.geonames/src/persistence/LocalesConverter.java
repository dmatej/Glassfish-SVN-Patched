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
package persistence;

import java.io.StringWriter;
import java.util.Locale;

import org.eclipse.persistence.mappings.DatabaseMapping;
import org.eclipse.persistence.mappings.converters.Converter;
import org.eclipse.persistence.sessions.Session;

/**
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
public class LocalesConverter implements Converter {

	public Object convertDataValueToObjectValue(Object dataValue, Session arg1) {
		if (dataValue == null || ((String) dataValue).length() == 0) {
			return new Locale[0];
		}

		String dataString = (String) dataValue;
		String[] localeValues = dataString.split(",");
		Locale[] locales = new Locale[localeValues.length];

		for (int index = 0; index < localeValues.length; index++) {
			locales[index] = createLocale(localeValues[index]);
		}

		return locales;
	}

	private Locale createLocale(String locValue) {
		if (locValue.length() <= 3) {
			return new Locale(locValue);
		}
		if (locValue.length() == 5 && (locValue.charAt(2) == '-' || locValue.charAt(2) == '_')) {
			return new Locale(locValue.substring(0, 2), locValue.substring(3));
		}

		// Handle special pt-BRR found in country info which should be pt-BR
		if (locValue.equals("pt-BRR")) {
			return createLocale("pt-BR");
		}
		throw new IllegalArgumentException(
				"LocalesConverter could not convert: " + locValue);
	}

	public Object convertObjectValueToDataValue(Object locales, Session arg1) {
		if (locales == null || ((Locale[]) locales).length == 0) {
			return null;
		}
		Locale[] locs = (Locale[]) locales;
		StringWriter writer = new StringWriter(locs.length * 2);
		for (int index = 0; index < locs.length; index++) {
			if (index > 0) {
				writer.write(",");
			}
			writer.write(locs[index].toString());
		}
		return writer.toString();
	}

	public void initialize(DatabaseMapping arg0, Session arg1) {
	}

	public boolean isMutable() {
		return false;
	}
}
