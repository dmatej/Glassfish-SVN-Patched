/*******************************************************************************
 * Copyright (c) 2010-2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *  dclarke - EclipseLink 2.3 - MySports Demo Bug 344608
 ******************************************************************************/
package example.mysports.tests.admin;

import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import junit.framework.Assert;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import example.mysports.MySportsConfig;
import example.mysports.admin.AdminServerConnector;
import example.mysports.admin.League;
import example.mysports.admin.Leagues;
import example.mysports.tests.TestingProperties;

/**
 * Tests for {@link AdminServerConnector}
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
public class AdminServerConnectorTests {

    private static MySportsConfig config;

    @Test
    public void getAllLeagues() throws Exception {
        Leagues leagues = config.getAdminConnector().getLeagues();

        Assert.assertNotNull(leagues);

        for (League league : leagues.getLeagues()) {
            System.out.println("League: " + league.getId() + " - " + league.getName());
        }

    }

    @Test
    public void verifyLeague_OSL() {
        League league = config.getAdminConnector().getLeague("OSL");

        Assert.assertNotNull(league);
        Assert.assertEquals(1l, league.getVersion());
    }

    @BeforeClass
    public static void setup() {
        config = new MySportsConfig();
        EntityManagerFactory emf = Persistence.createEntityManagerFactory("mysports-admin", TestingProperties.get());
        ((MockAdminServerConnector) config.getAdminConnector()).setEMF(emf);
    }

    @AfterClass
    public static void tearDown() {
        ((MockAdminServerConnector) config.getAdminConnector()).getEMF().close();
    }
}
