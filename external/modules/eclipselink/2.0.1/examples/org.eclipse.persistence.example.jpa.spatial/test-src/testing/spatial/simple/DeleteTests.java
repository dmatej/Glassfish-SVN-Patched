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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import javax.persistence.EntityManager;

import model.spatial.simple.SimpleSpatial;

/**
 * SQL samples from C:\oracle\db\10.2\md\demo\examples\eginsert.sql Note: Table
 * re-named from TEST81 to SIMPLE_SPATIAL
 */
public class DeleteTests extends SpatialTestCase {

    public void testAnySingleDelete() throws Exception {
        EntityManager em = getEntityManager();
        int initialQuantity = countSimpleSpatial(em);

        em.getTransaction().begin();

        SimpleSpatial ss = (SimpleSpatial) em.createQuery("SELECT ss FROM SimpleSpatial ss WHERE ss.id = (SELECT MIN(sss.id) FROM SimpleSpatial sss)").getSingleResult();
        assertNotNull("No SimpleSpatial found", ss);
        em.remove(ss);

        em.flush();

        int afterCount = countSimpleSpatial(em);

        assertEquals("Number of rows does not match", initialQuantity - 1, afterCount);

        em.getTransaction().rollback();
    }

}
