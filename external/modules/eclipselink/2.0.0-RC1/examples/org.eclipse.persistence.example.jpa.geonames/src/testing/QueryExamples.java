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
package testing;

import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;

import model.geonames.Point;

import org.junit.Test;

import utils.PersistenceHelper;

/**
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
public class QueryExamples extends BaseTestCase {

	@Test
	public void loadPEIPoints() throws Exception {
		EntityManager em = getEMF().createEntityManager();

		try {
			List<Point> points = em
					.createQuery(
							"SELECT p FROM Point p WHERE p.country.code = 'CA' AND p.admin1Code = '09'")
					.getResultList();

			System.out.println(" # PEI Points: " + points.size());
			for (Point point : points) {
				System.out.println("READ> " + point);
			}
		} finally {
			if (em.getTransaction().isActive()) {
				em.getTransaction().rollback();
			}
			em.close();
		}
	}
}
