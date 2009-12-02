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
 *      dclarke - Oracle Spatial Example (Bug 211007) Initial Contribution
 ******************************************************************************/
package testing.spatial.simple;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import javax.persistence.EntityManager;

import model.spatial.simple.SimpleSpatial;

import org.junit.Test;

import testing.spatial.simple.util.SampleGeometries;

public class UpdateTests extends SpatialTestCase {

    @Test
    public void testUpdateSingleToNull() throws Exception {
        EntityManager em = getEntityManager();

        em.getTransaction().begin();

        SimpleSpatial ss = (SimpleSpatial) em.createQuery("SELECT ss FROM SimpleSpatial ss WHERE ss.geometry IS NOT NULL").setMaxResults(1).getSingleResult();

        assertNotNull("No SimpleSpatial instances found", ss);

        ss.setGeometry(null);

        em.flush();
        em.getTransaction().rollback();
    }

    //@Test
    public void updateAllToNull() throws Exception {
        EntityManager em = getEntityManager();

        em.getTransaction().begin();
        int rowsUpdated = em.createQuery("UPDATE SimpleSpatial ss SET ss.geometry = NULL").executeUpdate();

        assertTrue(rowsUpdated > 0);

        em.flush();
        em.getTransaction().rollback();
    }

    @Test
    public void updateNullToNotNull() throws Exception {
        EntityManager em = getEntityManager();

        em.getTransaction().begin();
       List<SimpleSpatial> results = em.createQuery("SELECT ss FROM SimpleSpatial ss").getResultList();

        SampleGeometries samples = new SampleGeometries(0);
        for (SimpleSpatial ss : results) {
            if (ss.getGeometry() == null) {
                ss.setGeometry(samples.circle());
            } else {
                ss.setGeometry(samples.pointCluster2());
            }
        }

        em.flush();
        em.getTransaction().rollback();
    }

}
