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

import java.text.*;
import java.util.*;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;

import model.*;
import model.geonames.Country;
import model.geonames.Feature;
import model.geonames.Point;
import model.geonames.TimeZone;
import persistence.StringArrayConverter;
import utils.PersistenceHelper;

/**
 * 
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
public class PointsLoader {

	/**
	 * 
	 * @param countryCode
	 * @param em
	 * @param quantity
	 * @return
	 */
	public List<Point> load(String countryCode, EntityManager em, int quantity) {
		TabSeparatedRowReader reader = new TabSeparatedRowReader(countryCode
				.toUpperCase()
				+ ".txt");
		List<Point> points = new ArrayList<Point>();
		StringArrayConverter stringArrayConverter = new StringArrayConverter();

		// Pre-load Data 
		em.createQuery("SELECT tz FROM TimeZone tz WHERE tz.name LIKE 'America/%'").getResultList();

		int quantityRead = 0;
		while ((quantity == -1 || points.size() < quantity) && reader.ready()) {
			List<String> line = reader.readLine();

			int geonameid = new Integer(line.get(0));
			String name = line.get(1);
			String asciiname = line.get(2);

			String[] altNames = (String[]) stringArrayConverter
					.convertDataValueToObjectValue(line.get(3), null);

			double latitude = new Double(line.get(4));
			double longitude = new Double(line.get(5));

			Feature feature = null;
			if (em != null && line.get(6) != null && line.get(7) != null) {
				feature = em.find(Feature.class, new Feature.ID(line.get(6),
						line.get(7)));

			}

			Country country = null;
			if (em != null) {
				country = em.find(Country.class, line.get(8));
			}

			String[] altCCs = (String[]) stringArrayConverter
					.convertDataValueToObjectValue(line.get(9), null);

			String admin1Code = line.get(10);
			String admin2Code = line.get(11);
			String admin3Code = line.get(12);
			String admin4Code = line.get(13);
			int population = new Integer(line.get(14));

			Integer elevation = null;
			if (line.get(15) != null) {
				elevation = new Integer(line.get(15));
			}

			int gtopo30 = new Integer(line.get(16));

			TimeZone tz = null;
			if (em != null && line.get(17) != null) {
				tz = em.find(TimeZone.class, line.get(17));
			}

			Date modDate = null;
			try {
				modDate = new SimpleDateFormat("yyyy-mm-dd")
						.parse(line.get(18));
			} catch (ParseException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			Calendar modCal = Calendar.getInstance();
			modCal
					.set(modDate.getYear(), modDate.getMonth(), modDate
							.getDate());

			Point point = new Point(geonameid, name, asciiname, altNames,
					latitude, longitude, feature, country, altCCs, admin1Code,
					admin2Code, admin3Code, admin4Code, population, elevation,
					gtopo30, tz, modCal);
			em.persist(point);
			points.add(point);
		}
		reader.close();

		return points;
	}
}
