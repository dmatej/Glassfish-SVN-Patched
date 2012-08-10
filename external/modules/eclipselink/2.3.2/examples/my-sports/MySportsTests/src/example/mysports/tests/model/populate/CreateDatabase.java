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
package example.mysports.tests.model.populate;

import java.util.Iterator;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import junit.framework.Assert;

import org.eclipse.persistence.annotations.Multitenant;
import org.eclipse.persistence.config.PersistenceUnitProperties;
import org.eclipse.persistence.config.SessionCustomizer;
import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.descriptors.VPDMultitenantPolicy;
import org.eclipse.persistence.exceptions.IntegrityChecker;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.sessions.Session;
import org.eclipse.persistence.sessions.server.Server;
import org.eclipse.persistence.tools.schemaframework.DefaultTableGenerator;
import org.eclipse.persistence.tools.schemaframework.FieldDefinition;
import org.eclipse.persistence.tools.schemaframework.SchemaManager;
import org.eclipse.persistence.tools.schemaframework.TableCreator;
import org.eclipse.persistence.tools.schemaframework.TableDefinition;
import org.junit.Test;

import example.mysports.MySportsConfig;
import example.mysports.tests.TestingProperties;

/**
 * Create Schema and verify using integrity checker.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
public class CreateDatabase {

    private static final String FLEX_COLUM_PREFIX = "flex_";

    /**
     * Using EclipseLink's schema generation when the
     * {@link EntityManagerFactory} is created to drop and create the tables and
     * sequences.
     */
    @Test
    public void createMySportsSchema() throws Exception {
        EntityManagerFactory emf = null;
        try {
            emf = Persistence.createEntityManagerFactory(MySportsConfig.PU_NAME, TestingProperties.get());
            Server session = JpaHelper.getServerSession(emf);
            TableCreator tableCreator = new DefaultTableGenerator(session.getProject(), true).generateDefaultTableCreator();

            configureFlexFields(session, tableCreator);
            configureVPD(session, tableCreator);

            // Create Schema & sequences
            tableCreator.replaceTables(session);
            new SchemaManager(session).replaceSequences();
        } finally {
            if (emf != null) {
                emf.close();
            }
        }
    }


    /**
     * Verify that the database is empty with multi-tenancy disabled
     */
    @Test
    public void verifyEmpty() throws Exception {
        Map<String, Object> properties = TestingProperties.get();

        properties.put(PersistenceUnitProperties.SESSION_CUSTOMIZER, DisableMultitenancy.class.getName());

        EntityManagerFactory emf = null;
        try {
            emf = Persistence.createEntityManagerFactory(MySportsConfig.PU_NAME, properties);
            EntityManager em = emf.createEntityManager();

            try {
                Assert.assertEquals(0, em.createQuery("SELECT COUNT(o) FROM Player o", Number.class).getSingleResult().intValue());
                Assert.assertEquals(0, em.createQuery("SELECT COUNT(o) FROM Team o", Number.class).getSingleResult().intValue());
                Assert.assertEquals(0, em.createQuery("SELECT COUNT(o) FROM Division o", Number.class).getSingleResult().intValue());
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
    public void verifyMySportsSchema() {
        Map<String, Object> properties = TestingProperties.get();

        properties.put(PersistenceUnitProperties.SESSION_CUSTOMIZER, EnableIntegrityChecker.class.getName());
        properties.put(PersistenceUnitProperties.DEPLOY_ON_STARTUP, Boolean.TRUE.toString());

        EntityManagerFactory emf = null;

        try {
            emf = Persistence.createEntityManagerFactory(MySportsConfig.PU_NAME, properties);

            IntegrityChecker ic = JpaHelper.getServerSession(emf).getIntegrityChecker();
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

    /**
     * {@link SessionCustomizer} that disables {@link Multitenant}
     * configuration.
     * 
     * NOTE: If another {@link SessionCustomizer} is specified in the
     * persistence.xml this one will replace it.
     */
    public static class DisableMultitenancy implements SessionCustomizer {

        public void customize(Session session) throws Exception {
            for (ClassDescriptor desc : session.getDescriptors().values()) {
                desc.setMultitenantPolicy(null);
            }
        }

    }

    /**
     * 
     * @param session
     * @param tableCreator
     */
    private void configureVPD(Server session, TableCreator tableCreator) {
        // Configure VPD schema gen if configured
        for (ClassDescriptor desc : session.getDescriptors().values()) {
            if (desc.hasMultitenantPolicy() && desc.getMultitenantPolicy().getClass() == VPDMultitenantPolicy.class) {
                VPDMultitenantPolicy policy = (VPDMultitenantPolicy) desc.getMultitenantPolicy();
                TableDefinition tableDef = getTableDefinition(tableCreator, desc.getTableName());
                if (tableDef != null) {
                    // TODO: A bit hacky. What about multiple fields?
                    tableDef.setCreateVPDCalls(true, policy.getTenantDiscriminatorFields().keySet().iterator().next().getName());
                }
            }
        }
    }

    /**
     * TODO
     * 
     * @param session
     * @param tableCreator
     */
    private void configureFlexFields(Server session, TableCreator tableCreator) {

        addFlexFields(tableCreator, "mys_player", FLEX_COLUM_PREFIX, 5);
        addFlexFields(tableCreator, "mys_team", FLEX_COLUM_PREFIX, 5);
        addFlexFields(tableCreator, "mys_div", FLEX_COLUM_PREFIX, 5);

    }

    private void addFlexFields(TableCreator tableCreator, String name, String prefix, int num) {
        TableDefinition tableDef = getTableDefinition(tableCreator, name);

        if (tableDef == null) {
            throw new IllegalArgumentException("No table found to add columns named: " + name);
        }

        for (int index = 1; index <= num; index++) {
            String fieldName = prefix + index;
            FieldDefinition fieldDef = null;

            for (FieldDefinition fd : tableDef.getFields()) {
                if (fd.getName().equalsIgnoreCase(fieldName)) {
                    fieldDef = fd;
                }
            }
            if (fieldDef == null) {
                tableDef.addField(fieldName, String.class);
            }
        }
    }

    private TableDefinition getTableDefinition(TableCreator creator, String tableName) {
        for (Iterator<?> i = creator.getTableDefinitions().iterator(); i.hasNext();) {
            TableDefinition td = (TableDefinition) i.next();
            if (td.getName().equalsIgnoreCase(tableName)) {
                return td;
            }
        }
        return null;
    }
}
