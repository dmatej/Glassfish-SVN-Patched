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

import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import junit.framework.Assert;

import org.eclipse.persistence.config.PersistenceUnitProperties;
import org.eclipse.persistence.config.SessionCustomizer;
import org.eclipse.persistence.exceptions.IntegrityChecker;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.sessions.Session;
import org.eclipse.persistence.sessions.server.Server;
import org.eclipse.persistence.tools.schemaframework.SchemaManager;
import org.junit.Test;

import example.mysports.tests.TestingProperties;

/**
 * Create Schema and verify using integrity checker.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
public class CreateDatabase {

    /**
     * Using EclipseLink's schema generation when the
     * {@link EntityManagerFactory} is created to drop and create the tables and
     * sequences.
     */
    @Test
    public void createMySportsAdminSchema() throws Exception {
        EntityManagerFactory emf = null;

        try {
            emf = Persistence.createEntityManagerFactory("mysports-admin", TestingProperties.get());

            Server session = JpaHelper.getServerSession(emf);
            SchemaManager sm = new SchemaManager(session);
            sm.replaceDefaultTables();
            sm.createSequences();
        } finally {
            if (emf != null) {
                emf.close();
            }
        }
    }

    /**
     * Verify that the database is empty with multitenancy disabled
     */
    //@Test
    public void verifyEmpty() throws Exception {
        EntityManagerFactory emf = null;
        try {
            emf = Persistence.createEntityManagerFactory("mysports-admin", TestingProperties.get());

            EntityManager em = emf.createEntityManager();

            try {
                Assert.assertEquals(0, em.createQuery("SELECT COUNT(o) FROM League o", Number.class).getSingleResult().intValue());
            } finally {
                em.close();
            }
        } finally {
            if (emf != null) {
                emf.close();
            }
        }
    }

    /**
     * Verify the schema using the integrity checker to compare database
     * structure to that expected in the mappings.
     */
    @Test
    public void verifyMySportsAdminSchema() {
        Map<String, Object> properties = TestingProperties.get();

        properties.put(PersistenceUnitProperties.SESSION_CUSTOMIZER, EnableIntegrityChecker.class.getName());
        properties.put(PersistenceUnitProperties.DEPLOY_ON_STARTUP, Boolean.TRUE.toString());

        EntityManagerFactory emf = null;
        try {
            emf = Persistence.createEntityManagerFactory("mysports-admin", properties);

            Server session = JpaHelper.getServerSession(emf);
            IntegrityChecker ic = session.getIntegrityChecker();
            Assert.assertNotNull(ic);
            Assert.assertTrue(ic.getCaughtExceptions().isEmpty());
        } finally {
            if (emf != null) {
                emf.close();
            }
        }
    }

    /**
     * {@link SessionCustomizer} that enables the {@link IntegrityChecker}.
     * 
     * NOTE: If another {@link SessionCustomizer} is specified in the
     * persistence.xml this one will replace it.
     */
    public static class EnableIntegrityChecker implements SessionCustomizer {

        public void customize(Session session) throws Exception {
            IntegrityChecker ic = new IntegrityChecker();
            ic.setShouldCheckDatabase(true);
            session.setIntegrityChecker(ic);
        }

    }
}
