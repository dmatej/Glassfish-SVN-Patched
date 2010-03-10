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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.EntityManager;

import example.where.model.geonames.Feature;


/**
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
public class FeatureLoader {
	
	protected static final String FILE_NAME = "featureCodes.txt";

	public List<Feature> load(EntityManager em) {
		List<Feature> features = new ArrayList<Feature>();
		TabSeparatedRowReader reader = new TabSeparatedRowReader(
				"featureCodes.txt");
		//reader.skipLines(1);

		while (reader.ready()) {
			List<String> line = reader.readLine();

			String code = line.get(0);
			if (!code.equalsIgnoreCase("null")) {
				Feature newFeature = new Feature(code.substring(0, 1),code.substring(2), line.get(1),
						line.size() > 2 ? line.get(2) : null);
				
				if (em != null) {
					em.persist(newFeature);
				}
				
				features.add(newFeature);
			}
		}
		reader.close();
		return features;
	}

}
