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
package testing.spatial.simple.util;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.EntityManager;

import model.spatial.simple.SimpleSpatial;
import oracle.spatial.geometry.JGeometry;

/**
 * Helper class to read and compare SimpleSpatial results read using SQL versus
 * those read using EclipseLink.
 * 
 * @author dclarke
 */
public class SQLReader {
    private String sql;
    private List<SimpleSpatial> results;

    public SQLReader(EntityManager em, String sql) throws SQLException {
        this.sql = sql;
        readResults(em);
    }

    private void readResults(EntityManager em) throws SQLException {
        List<Object[]> rawResults = em.createNativeQuery(getSql()).getResultList();
        this.results = new ArrayList<SimpleSpatial>(rawResults.size());

        for (Object[] rawResult : rawResults) {
            long gid = ((Number) rawResult[0]).longValue();
            JGeometry geometry = (JGeometry) rawResult[1];
            this.results.add(new SimpleSpatial(gid, geometry));
        }
    }

    public String getSql() {
        return sql;
    }

    public List<SimpleSpatial> getResults() {
        return results;
    }

    /**
     * Return NULL if they match or an error message if they do not.
     */
    public String compare(List<SimpleSpatial> values) {
        if (getResults() == null && values == null) {
            return null;
        }
        if (getResults() == null || values == null) {
            return "SQL = " + getResults() + " - TopLink = " + values;
        }
        if (getResults().size() != values.size()) {
            return "SQL size = " + getResults().size() + " - TopLink size = " + values.size();
        }

        for (int index = 0; index < getResults().size(); index++) {
            SimpleSpatial sqlGeometry = getResults().get(index);
            SimpleSpatial tlGeometry = values.get(index);

            String compareResult = compare(sqlGeometry, tlGeometry);
            if (compareResult != null) {
                return compareResult;
            }
        }

        return null;
    }

    @SuppressWarnings("deprecation")
    public String compare(SimpleSpatial sqlGeometry, SimpleSpatial tlGeometry) {
        if (sqlGeometry == tlGeometry) {
            return null;
        }
        if (sqlGeometry == null || tlGeometry == null) {
            return "SQL: " + sqlGeometry + "does not equal: " + tlGeometry;
        }
        if (sqlGeometry.getId() != tlGeometry.getId()) {
            return "SQL: " + sqlGeometry + "does not equal: " + tlGeometry;
        }
        if (sqlGeometry.getGeometry() == null && tlGeometry.getGeometry() == null) {
            return null;
        }
        if (sqlGeometry.getGeometry() == null || tlGeometry.getGeometry() == null) {
            return "SQL: " + sqlGeometry + "does not equal: " + tlGeometry;
        }
        if (!sqlGeometry.getGeometry().equals(tlGeometry.getGeometry())) {
            return "SQL: " + sqlGeometry + "does not equal: " + tlGeometry;
        }
        return null;
    }
}
