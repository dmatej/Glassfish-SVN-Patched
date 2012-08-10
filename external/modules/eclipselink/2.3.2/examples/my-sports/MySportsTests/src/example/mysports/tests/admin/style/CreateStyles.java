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
package example.mysports.tests.admin.style;

import java.io.InputStream;
import java.io.StringWriter;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import junit.framework.Assert;

import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.sessions.server.Server;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import example.mysports.admin.model.Style;
import example.mysports.tests.TestingProperties;

/**
 * Create initial styles.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
public class CreateStyles {

    private static EntityManagerFactory emf;

    @Test
    public void verifySession() {
        Assert.assertNotNull(emf);

        Server session = JpaHelper.getServerSession(emf);
        Assert.assertNotNull(session);
        Assert.assertTrue(session.isServerSession());
    }

    @Test
    public void createStyles() throws Exception {
        EntityManager em = emf.createEntityManager();

        try {
            em.getTransaction().begin();

            em.persist(new Style("default", load("default")));
            em.persist(new Style("red", load("red")));
            em.persist(new Style("black", load("black")));
            em.persist(new Style("blue", load("blue")));
            em.persist(new Style("green", load("green")));

            em.getTransaction().commit();
        } finally {
            em.close();
        }
    }

    @BeforeClass
    public static void createEMF() {
        emf = Persistence.createEntityManagerFactory("mysports-admin", TestingProperties.get());

        EntityManager em = emf.createEntityManager();
        try {
            em.getTransaction().begin();
            em.createQuery("DELETE FROM Style").executeUpdate();
            em.getTransaction().commit();
        } finally {
            em.close();
        }
    }

    @AfterClass
    public static void closeEMF() {
        emf.close();
    }

    private String load(String name) throws Exception {
        InputStream in = Thread.currentThread().getContextClassLoader().getResourceAsStream("example/mysports/tests/admin/style/" + name + ".css");
        StringWriter writer = new StringWriter(in.available());
        for (int ch = in.read(); ch >= 0; ch = in.read() ) {
            writer.write(ch);
        }
        in.close();
        return writer.toString();
    }
}
