/*******************************************************************************
 * Copyright (c) 2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 * 		dclarke - 
 ******************************************************************************/
package example.mysports.tests.admin.extensions;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import org.eclipse.persistence.jpa.JpaHelper;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import example.mysports.admin.jaxrs.Extensions;
import example.mysports.tests.TestingProperties;

public class JaxrsExtensionsTests {

    private static Extensions extensions;

    @Test
    public void create() throws Exception {
        extensions.create("Player", "OSL", "test", "java.lang.String", "TEST", "test");
    }
    
    @BeforeClass
    public static void setup() {
        EntityManagerFactory emf = Persistence.createEntityManagerFactory("mysports-admin", TestingProperties.get());
        extensions = new Extensions();
        extensions.setEmf(emf);
    }
    
    @AfterClass
    public static void tearDown() {
        extensions.getEmf().close();
    }
    
    @After
    public void deleteTestdata() {
        JpaHelper.getServerSession(extensions.getEmf()).executeNonSelectingSQL("DELETE FROM MYS_ADMIN_EXT WHERE name = 'test'");
    }
}

