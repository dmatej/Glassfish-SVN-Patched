/*******************************************************************************
 * Copyright (c) 1998, 2008 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     dclarke - Bug 277731: Simple Map Dynamic JPA Example
 *               http://wiki.eclipse.org/EclipseLink/Examples/JPA/Dynamic/SimpleDynamicMap
 ******************************************************************************/
package testing.jpa;

import static example.jpa.DynamicMapWithRelationshipsExample.TYPE_A;
import static example.jpa.DynamicMapWithRelationshipsExample.TYPE_B;
import static example.jpa.DynamicMapWithRelationshipsExample.TYPE_C;
import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertNotNull;
import static junit.framework.Assert.assertTrue;
import static junit.framework.Assert.fail;

import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.mappings.DirectToFieldMapping;
import org.eclipse.persistence.sessions.factories.SessionManager;
import org.eclipse.persistence.sessions.server.Server;
import org.junit.AfterClass;
import org.junit.Test;

import example.jpa.DynamicMapWithRelationshipsExample;
import example.jpa.DynamicMapHelper;
import example.util.ExamplePropertiesLoader;

public class DynamicMapWithRelationships_Tests {

    private static DynamicMapWithRelationshipsExample example = new DynamicMapWithRelationshipsExample();

    private static EntityManagerFactory emf;

    private static EntityManagerFactory getEMF() throws Exception {
        if (emf == null) {
            Map<String, Object> properties = new HashMap<String, Object>();
            ExamplePropertiesLoader.loadProperties(properties);
            emf = Persistence.createEntityManagerFactory("dynamic-simple-map", properties);

            assertNotNull(emf);
            assertTrue(emf.isOpen());

            Server session = JpaHelper.getServerSession(emf);
            assertNotNull(session);
            assertTrue(session.isConnected());
            assertEquals(0, session.getDescriptors().size());

            createDynamicTypes();
        }
        assertNotNull("No EntityManagerFactory returned from createEMF", emf);
        return emf;
    }

    private static void createDynamicTypes() throws Exception {
        example.createDynamicTypes(getEMF());
        Server session = JpaHelper.getServerSession(emf);
        assertEquals(3, session.getDescriptors().size());

        ClassDescriptor descriptor = DynamicMapHelper.getDescriptor(getEMF(), TYPE_A);
        assertNotNull(descriptor);
        assertEquals("model." + TYPE_A, descriptor.getJavaClassName());
        assertEquals(TYPE_A, descriptor.getAlias());
        assertEquals("DYNAMIC_A", descriptor.getTableName());
        assertEquals(1, descriptor.getPrimaryKeyFieldNames().size());
        assertEquals("DYNAMIC_A.A_ID", descriptor.getPrimaryKeyFieldNames().get(0));
        assertEquals(4, descriptor.getMappings().size());
        DirectToFieldMapping mapping = (DirectToFieldMapping) descriptor.getMappingForAttributeName("id");
        assertNotNull(mapping);
        assertEquals("DYNAMIC_A.A_ID", mapping.getFieldName());
        assertEquals(Integer.class, mapping.getAttributeClassification());
        mapping = (DirectToFieldMapping) descriptor.getMappingForAttributeName("value");
        assertNotNull(mapping);
        assertEquals("DYNAMIC_A.VALUE", mapping.getFieldName());
        assertEquals(String.class, mapping.getAttributeClassification());

        descriptor = DynamicMapHelper.getDescriptor(getEMF(), TYPE_B);
        assertNotNull(descriptor);
        assertEquals("model." + TYPE_B, descriptor.getJavaClassName());
        assertEquals(TYPE_B, descriptor.getAlias());
        assertEquals("DYNAMIC_B", descriptor.getTableName());
        assertEquals(1, descriptor.getPrimaryKeyFieldNames().size());
        assertEquals("DYNAMIC_B.B_ID", descriptor.getPrimaryKeyFieldNames().get(0));
        assertEquals(3, descriptor.getMappings().size());
        mapping = (DirectToFieldMapping) descriptor.getMappingForAttributeName("id");
        assertNotNull(mapping);
        assertEquals("DYNAMIC_B.B_ID", mapping.getFieldName());
        assertEquals(Integer.class, mapping.getAttributeClassification());
        mapping = (DirectToFieldMapping) descriptor.getMappingForAttributeName("value");
        assertNotNull(mapping);
        assertEquals("DYNAMIC_B.VALUE", mapping.getFieldName());
        assertEquals(Calendar.class, mapping.getAttributeClassification());

        descriptor = DynamicMapHelper.getDescriptor(getEMF(), TYPE_C);
        assertNotNull(descriptor);
        assertEquals("model." + TYPE_C, descriptor.getJavaClassName());
        assertEquals(TYPE_C, descriptor.getAlias());
        assertEquals("DYNAMIC_C", descriptor.getTableName());
        assertEquals(1, descriptor.getPrimaryKeyFieldNames().size());
        assertEquals("DYNAMIC_C.C_ID", descriptor.getPrimaryKeyFieldNames().get(0));
        assertEquals(2, descriptor.getMappings().size());
        mapping = (DirectToFieldMapping) descriptor.getMappingForAttributeName("id");
        assertNotNull(mapping);
        assertEquals("DYNAMIC_C.C_ID", mapping.getFieldName());
        assertEquals(Integer.class, mapping.getAttributeClassification());
        mapping = (DirectToFieldMapping) descriptor.getMappingForAttributeName("value");
        assertNotNull(mapping);
        assertEquals("DYNAMIC_C.VALUE", mapping.getFieldName());
        assertEquals(byte[].class, mapping.getAttributeClassification());
    }

    @Test
    public void persistSimpleTypeInstances() throws Exception {
        EntityManager em = getEMF().createEntityManager();
        assertEquals(0, ((Number) em.createQuery("SELECT COUNT(a) FROM SimpleTypeA a").getSingleResult()).intValue());
        assertEquals(0, ((Number) em.createQuery("SELECT COUNT(b) FROM SimpleTypeB b").getSingleResult()).intValue());
        assertEquals(0, ((Number) em.createQuery("SELECT COUNT(c) FROM SimpleTypeC c").getSingleResult()).intValue());

        example.persistDynamicInstances(getEMF());

        assertEquals(1, ((Number) em.createQuery("SELECT COUNT(a) FROM SimpleTypeA a").getSingleResult()).intValue());
        assertEquals(1, ((Number) em.createQuery("SELECT COUNT(b) FROM SimpleTypeB b").getSingleResult()).intValue());
        assertEquals(2, ((Number) em.createQuery("SELECT COUNT(c) FROM SimpleTypeC c").getSingleResult()).intValue());

        Map entityA = (Map) em.find(DynamicMapHelper.getClass(getEMF(), TYPE_A), 1);

        assertNotNull(entityA);

        List<Map<String, Object>> bs = (List<Map<String, Object>>) entityA.get("bs");
        assertNotNull(bs);
        assertEquals(1, bs.size());

        List<Map<String, Object>> cs = (List<Map<String, Object>>) entityA.get("cs");
        assertNotNull(cs);
        assertEquals(2, cs.size());

        em.close();
    }

    @Test
    public void querySimpleTypeInstances() throws Exception {
        List<Map> entities = example.queryDynamicInstances(getEMF());

        assertNotNull(entities);
        assertEquals(1, entities.size());
        assertEquals(1, entities.get(0).get("id"));
        assertEquals("value-1", entities.get(0).get("value"));

        EntityManager em = getEMF().createEntityManager();

        List<Map<String, Object>> allBs = em.createQuery("SELECT b FROM SimpleTypeB b").getResultList();
        assertNotNull(allBs);
        assertEquals(1, allBs.size());

        List<Map<String, Object>> allCs = em.createQuery("SELECT c FROM SimpleTypeC c").getResultList();
        assertNotNull(allCs);
        assertEquals(2, allCs.size());

        em.close();
    }

    @Test
    public void updateSimpleTypeInstances() throws Exception {
        example.updateDyanmicInstances(getEMF());

        JpaHelper.getServerSession(getEMF()).getIdentityMapAccessor().initializeAllIdentityMaps();
        EntityManager em = getEMF().createEntityManager();

        Map entityA = (Map) em.find(DynamicMapHelper.getClass(getEMF(), TYPE_A), 1);
        assertNotNull(entityA);
        assertEquals(1, entityA.get("id"));
        assertEquals("value-1+", entityA.get("value"));

        em.close();
    }

    @Test
    public void deleteSimpleTypeInstances() throws Exception {
        example.deleteDynamicInstances(getEMF());

        EntityManager em = getEMF().createEntityManager();

        assertEquals(0, ((Number) em.createQuery("SELECT COUNT(a) FROM SimpleTypeA a").getSingleResult()).intValue());
        assertEquals(0, ((Number) em.createQuery("SELECT COUNT(b) FROM SimpleTypeB b").getSingleResult()).intValue());
        assertEquals(0, ((Number) em.createQuery("SELECT COUNT(c) FROM SimpleTypeC c").getSingleResult()).intValue());

        em.close();
    }

    @Test
    public void removeSimpleType() throws Exception {
        example.removeDynamicTypes(getEMF());

        assertEquals(0, JpaHelper.getServerSession(getEMF()).getDescriptors().size());

        try {
            DynamicMapHelper.getDescriptor(getEMF(), TYPE_A);
        } catch (IllegalArgumentException iae) {
            return;
        }
        fail("Expected IllegalArgumentException not thrown");
    }

    @AfterClass
    public static void closeEMF() {
        if (emf != null && emf.isOpen()) {
            emf.close();
        }
        SessionManager.getManager().destroyAllSessions();
    }

}
