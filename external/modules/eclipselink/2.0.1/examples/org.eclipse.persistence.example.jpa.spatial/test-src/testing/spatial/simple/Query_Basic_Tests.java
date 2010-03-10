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

import static org.junit.Assert.*;

import java.util.List;
import java.util.Vector;

import javax.persistence.EntityManager;

import model.spatial.simple.SimpleSpatial;
import oracle.spatial.geometry.JGeometry;

import org.eclipse.persistence.expressions.*;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.queries.ReadAllQuery;
import org.junit.Test;

import testing.spatial.simple.util.SQLReader;

/**
 * Query tests that do not involve using a spatial operator in the selection
 * criteria.
 */
public class Query_Basic_Tests extends SpatialTestCase {

    @Test
    public void testReadAll() throws Exception {
        EntityManager em = getEntityManager();

        String sql = "select GID, GEOMETRY, NAME from SIMPLE_SPATIAL ORDER BY GID";
        SQLReader reader = new SQLReader(em, sql);

        List<SimpleSpatial> results = (List) em.createQuery("SELECT ss FROM SimpleSpatial ss ORDER BY ss.id").getResultList();

        String compareResult = reader.compare(results);

        assertNull(compareResult, compareResult);
    }

    /**
     * Comparisons to geometry != NULL always fail.
     */
    public void testReadNotNullGeometry() throws Exception {
        EntityManager em = getEntityManager();

        ReadAllQuery roq = new ReadAllQuery(SimpleSpatial.class);
        ExpressionBuilder eb = roq.getExpressionBuilder();
        roq.setSelectionCriteria(eb.get("geometry").notNull());

        List<SimpleSpatial> results = (List) JpaHelper.createQuery(roq, em).getSingleResult();

        assertNotNull(results);
        int countAll = countSimpleSpatial(em);

        assertEquals("More then one found", countAll - 1, results.size());
    }

    public void testRawUsage() throws Exception {
        EntityManager em = getEntityManager();

        Vector stringsVec = new Vector();
        stringsVec.add("SDO_WITHIN_DISTANCE("); // Geometry 1
        stringsVec.add(","); // Geometry 2
        stringsVec.add(","); // PARAMS
        stringsVec.add(")");

        ExpressionOperator op = new ExpressionOperator(-1, stringsVec);
        op.bePrefix();

        ReadAllQuery raq = new ReadAllQuery(SimpleSpatial.class);
        ExpressionBuilder eb = raq.getExpressionBuilder();

        JGeometry comparison = JGeometry.createMultiPoint(new Object[] { new double[] { 5, 6 }, new double[] { 7, 8 } }, 2, 0);

        Vector args = new Vector(2);
        args.add(comparison);
        args.add("DISTANCE=10");

        Expression criteria = eb.get("geometry").performOperator(op, args).equal("TRUE");

        raq.setSelectionCriteria(criteria);

        JpaHelper.createQuery(raq, em).getResultList();
    }

}
