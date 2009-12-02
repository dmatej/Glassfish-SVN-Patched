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

import example.where.model.geonames.TimeZone;


public class TimeZoneLoader {
	
	protected static final String FILE_NAME = "timeZones.txt";

	public List<TimeZone> load(EntityManager em) {
		List<TimeZone> timezones = new ArrayList<TimeZone>();
		TabSeparatedRowReader reader = new TabSeparatedRowReader(
				FILE_NAME);
		reader.skipLines(1);

		while (reader.ready()) {
			List<String> line = reader.readLine();
			TimeZone newTZ = new TimeZone(line.get(0), new Double(line.get(1)),
					new Double(line.get(2)));
			
			if (em != null) {
				em.persist(newTZ);
			}
			
			timezones.add(newTZ);
		}
		reader.close();

		return timezones;
	}
}
