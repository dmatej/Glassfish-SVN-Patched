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
import java.util.*;

import javax.persistence.EntityManager;

import model.geonames.Language;

/**
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
public class LanguageLoader {

	public static void main(String[] args) throws IOException {
		LanguageLoader loader = new LanguageLoader();
		loader.load(null);
		
		for (Language language : loader.getIso639_3_Languages().values()) {
			System.out.println(language.toString());
		}
	}
	
	private Map<String, Language> iso639_3_Languages;
	private Map<String, Language> iso639_1_Languages;
	
	public Map<String, Language> getIso639_3_Languages() {
		return this.iso639_3_Languages;
	}

	public Map<String, Language> getIso639_1_Languages() {
		return this.iso639_1_Languages;
	}

	public void load(EntityManager em) {
		TabSeparatedRowReader reader = new TabSeparatedRowReader(
				"iso-languagecodes.txt");
		reader.skipLines(1);

		this.iso639_1_Languages = new HashMap<String, Language>();
		this.iso639_3_Languages = new HashMap<String, Language>();
		
		while (reader.ready()) {
			List<String> tokens = reader.readLine();
			Language newLang = new Language(tokens.get(0), tokens.get(1),
					tokens.get(2), tokens.get(3));
			if (em != null) {
				em.persist(newLang);
			}

			getIso639_1_Languages().put(newLang.getCode1(), newLang);
			getIso639_3_Languages().put(newLang.getCode3(), newLang);
		}
		reader.close();
	}
}
