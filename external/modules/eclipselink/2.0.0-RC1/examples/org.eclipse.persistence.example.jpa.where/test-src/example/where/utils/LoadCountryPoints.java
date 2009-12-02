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
package example.where.utils;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;

import example.where.service.impl.EclipseLinkServiceFactory;
import example.where.service.impl.PersistenceService;
import example.where.utils.data.PointsLoader;

/**
 * 
 * @author dclarke
 * @since EclipseLink 1.1
 */
public class LoadCountryPoints {

    public static void main(String[] args) {
        EntityManagerFactory emf = PersistenceService.createEMF(null, true, new EclipseLinkServiceFactory(), false);
        EntityManager em = emf.createEntityManager();

        try {
            PointsLoader.load("CA", em, 50000);
            PointsLoader.load("GB", em, 50000);
            PointsLoader.load("CZ", em, 50000);
            PointsLoader.load("US", em, 50000);
        } finally {
            if (em.getTransaction().isActive()) {
                System.err.println("Rolling back tx");
                em.getTransaction().rollback();
            }
            em.close();
            emf.close();
        }
    }

}
