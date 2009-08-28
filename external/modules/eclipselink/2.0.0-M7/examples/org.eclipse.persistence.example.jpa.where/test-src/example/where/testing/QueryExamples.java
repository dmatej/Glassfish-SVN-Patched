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
package example.where.testing;

import java.util.List;

import junit.framework.Assert;

import org.junit.Test;

import example.where.model.geonames.AdminDivision;
import example.where.model.geonames.Point;
import example.where.service.LocationService;

/**
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
public class QueryExamples extends BaseTestCase {

    @Test
    public void loadPEIPoints() throws Exception {
        LocationService locService = getServiceFactory().createLocationService();

        List<Point> points = locService.getPoints("CA", "09");

        System.out.println(" # PEI Points: " + points.size());
        for (Point point : points) {
            System.out.println("READ> " + point);
        }
    }

    @Test
    public void loadLakesidePoints() throws Exception {
        LocationService locService = getServiceFactory().createLocationService();

        List<Point> points = locService.getPoints("Lakeside", "CA", "09");

        Assert.assertEquals(1, points.size());
        Point point = points.get(0);
        System.out.println("READ> " + point);
        System.out.println("\tFEATURE: " + point.getFeature().getName() + " - " + point.getFeature().getDescription());

        AdminDivision pei = point.getAdminDivision();
        System.out.println("\tPEI: " + pei);
        System.out.println("\tCountry: " + pei.getCountry());
    }

}
