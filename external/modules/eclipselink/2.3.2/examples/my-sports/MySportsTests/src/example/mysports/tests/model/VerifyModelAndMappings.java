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
package example.mysports.tests.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.internal.helper.Helper;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.mappings.DatabaseMapping;
import org.eclipse.persistence.sessions.server.Server;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import example.mysports.MySportsConfig;
import example.mysports.model.Division;
import example.mysports.model.Player;
import example.mysports.model.Team;
import example.mysports.tests.TestingProperties;
/**
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
public class VerifyModelAndMappings {

    private static EntityManagerFactory emf;

    @Test
    public void verifySession() {
        Server session = JpaHelper.getServerSession(emf);
        assertNotNull(session);
        assertTrue(session.isServerSession());
    }

    @Test
    public void verifyPlayer() {
        Server session = JpaHelper.getServerSession(emf);
        ClassDescriptor descriptor = session.getClassDescriptor(Player.class);

        assertNotNull(descriptor);
        assertEquals(Helper.getShortClassName(descriptor.getJavaClass()), descriptor.getAlias());

        //assertTrue(Extensible.class.isAssignableFrom(descriptor.getJavaClass()));
        
        // Verify mappings
        assertEquals(8, descriptor.getMappings().size());
        DatabaseMapping mapping;
        
        mapping = descriptor.getMappingForAttributeName("id");
        assertNotNull(mapping);
        assertTrue(mapping.isDirectToFieldMapping());
        
        // Verify Weaving's marker interfaces
    }

    @Test
    public void verifyTeam() {
        Server session = JpaHelper.getServerSession(emf);
        ClassDescriptor descriptor = session.getClassDescriptor(Team.class);

        assertNotNull(descriptor);
        assertEquals(Helper.getShortClassName(descriptor.getJavaClass()), descriptor.getAlias());
    }

    @Test
    public void verifyDivision() {
        Server session = JpaHelper.getServerSession(emf);
        ClassDescriptor descriptor = session.getClassDescriptor(Division.class);

        assertNotNull(descriptor);
        assertEquals(Helper.getShortClassName(descriptor.getJavaClass()), descriptor.getAlias());
    }

    @BeforeClass
    public static void createEMF() {
        emf = Persistence.createEntityManagerFactory(MySportsConfig.PU_NAME, TestingProperties.get());
    }

    @AfterClass
    public static void closeEMF() {
        emf.close();
    }
}
