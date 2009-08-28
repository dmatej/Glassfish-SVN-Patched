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
package utils;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;

import model.geonames.Continent;
import utils.load.*;

/**
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
public class LoadCanada {

	public static void main(String[] args) {
		EntityManagerFactory emf = PersistenceHelper.createEMF(null, true);
		EntityManager em = emf.createEntityManager();

		try {
			em.getTransaction().begin();
			
			// Pre-load Data to optimize relationship creation 
			em.createQuery("SELECT tz FROM TimeZone tz WHERE tz.name LIKE 'America/%'").getResultList();

			new PointsLoader().load("CA", em, -1);

			em.getTransaction().commit();
		} finally {
			if (em.getTransaction().isActive()) {
				em.getTransaction().rollback();
			}
			em.close();
			emf.close();
		}
	}

}
