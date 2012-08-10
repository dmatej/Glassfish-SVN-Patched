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
 *      dclarke - Bug 356928: Multi-Tenancy with VPD Example
 ******************************************************************************/
package tests;

import static org.junit.Assert.assertTrue;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import model.Task;

import org.eclipse.persistence.internal.descriptors.PersistenceEntity;
import org.eclipse.persistence.internal.descriptors.PersistenceObject;
import org.eclipse.persistence.internal.weaving.PersistenceWeaved;
import org.eclipse.persistence.internal.weaving.PersistenceWeavedChangeTracking;
import org.eclipse.persistence.internal.weaving.PersistenceWeavedFetchGroups;
import org.eclipse.persistence.internal.weaving.PersistenceWeavedLazy;
import org.eclipse.persistence.queries.FetchGroupTracker;
import org.junit.AfterClass;
import org.junit.Test;

/**
 * Base test case handling {@link EntityManagerFactory} creation and close as
 * well as user specific {@link EntityManager} creation.
 * 
 * @author dclarke
 * @since EclipseLInk 2.3.1
 */
public abstract class BaseJPATest {

    private static EntityManagerFactory emf;

    @Test
    public void verifyWeaving() {
        assertTrue("Entity class does not implement PersistenceEntity", PersistenceEntity.class.isAssignableFrom(Task.class));
        assertTrue("Entity class does not implement PersistenceObject", PersistenceObject.class.isAssignableFrom(Task.class));
        assertTrue("Entity class does not implement PersistenceObject", PersistenceWeaved.class.isAssignableFrom(Task.class));
        assertTrue("Entity class does not implement PersistenceObject", PersistenceWeavedChangeTracking.class.isAssignableFrom(Task.class));
        assertTrue("Entity class does not implement PersistenceObject", PersistenceWeavedFetchGroups.class.isAssignableFrom(Task.class));
        assertTrue("Entity class does not implement PersistenceObject", PersistenceWeavedLazy.class.isAssignableFrom(Task.class));
        assertTrue("Entity class does not implement FetchGroupTracker", FetchGroupTracker.class.isAssignableFrom(Task.class));
    }

    protected EntityManagerFactory getEMF() {
        if (emf == null) {
            emf = createEMF();
            populate();
        }
        return emf;
    }

    protected EntityManagerFactory createEMF() {
        return Persistence.createEntityManagerFactory("todoList");
    }

    @AfterClass
    public static void closeEMF() {
        if (emf != null && emf.isOpen()) {
            emf.close();
        }
    }

    protected abstract void populate();
}
