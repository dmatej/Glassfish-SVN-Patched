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

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.internal.helper.Helper;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.sessions.server.Server;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import example.mysports.admin.model.Extension;
import example.mysports.admin.model.HostedLeague;
import example.mysports.admin.model.Style;
import example.mysports.tests.TestingProperties;

/**
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
public class VerifyModelAndMappings {

    private static EntityManagerFactory emf;

    @Test
    public void verifySession() {
        Assert.assertNotNull(emf);

        Server session = JpaHelper.getServerSession(emf);
        Assert.assertNotNull(session);
        Assert.assertTrue(session.isServerSession());
    }

    @Test
    public void verifyLeague() {
        Server session = JpaHelper.getServerSession(emf);
        ClassDescriptor descriptor = session.getClassDescriptor(HostedLeague.class);

        Assert.assertNotNull(descriptor);
        Assert.assertEquals(Helper.getShortClassName(descriptor.getJavaClass()), descriptor.getAlias());
    }

    @Test
    public void verifyStyle() {
        Server session = JpaHelper.getServerSession(emf);
        ClassDescriptor descriptor = session.getClassDescriptor(Style.class);

        Assert.assertNotNull(descriptor);
        Assert.assertEquals(Helper.getShortClassName(descriptor.getJavaClass()), descriptor.getAlias());
    }

    @Test
    public void verifyExtension() {
        Server session = JpaHelper.getServerSession(emf);
        ClassDescriptor descriptor = session.getClassDescriptor(Extension.class);

        Assert.assertNotNull(descriptor);
        Assert.assertEquals(Helper.getShortClassName(descriptor.getJavaClass()), descriptor.getAlias());
    }

    @BeforeClass
    public static void createEMF() {
        emf = Persistence.createEntityManagerFactory("mysports-admin", TestingProperties.get());
    }

    @AfterClass
    public static void closeEMF() {
        emf.close();
    }
}
