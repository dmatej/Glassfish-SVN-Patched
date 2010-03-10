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
package testing.nativeorm;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertNotNull;
import static junit.framework.Assert.assertNull;
import static junit.framework.Assert.assertTrue;

import java.util.List;
import java.util.Map;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.expressions.ExpressionBuilder;
import org.eclipse.persistence.mappings.DirectToFieldMapping;
import org.eclipse.persistence.queries.ReportQuery;
import org.eclipse.persistence.sessions.DatabaseSession;
import org.eclipse.persistence.sessions.Session;
import org.junit.AfterClass;
import org.junit.Test;

import example.nativeorm.ExampleHelper;
import example.nativeorm.SimpleDynamicMap_NativeExample;

public class DynamicMap_Tests {

    private static SimpleDynamicMap_NativeExample example = new SimpleDynamicMap_NativeExample();

    private static DatabaseSession session;

    @AfterClass
    public static void logoutSession() {
        if (session != null && session.isConnected()) {
            session.logout();
        }
    }

    private static DatabaseSession getSession() {
        if (session == null) {
            session = ExampleHelper.createSession();

            assertNotNull(session);
            assertTrue(session.isConnected());
            assertEquals(0, session.getDescriptors().size());

            createSimpleType();
        }
        assertNotNull("No session returned from createSession", session);
        return session;
    }

    private static ClassDescriptor getDescriptor(String alias) {
        ClassDescriptor descriptor = getSession().getClassDescriptorForAlias(alias);
        assertNotNull("No descriptor found for alias: " + alias, descriptor);
        return descriptor;
    }

    private static void createSimpleType() {
        ClassDescriptor descriptor = ExampleHelper.createDynamicType(getSession());

        assertNotNull(descriptor);
        assertEquals(1, session.getDescriptors().size());
        assertEquals("model.SimpleType", descriptor.getJavaClassName());
        assertEquals("SimpleType", descriptor.getAlias());
        assertEquals("DYNAMIC_SIMPLE", descriptor.getTableName());
        assertEquals(1, descriptor.getPrimaryKeyFieldNames().size());
        assertEquals("DYNAMIC_SIMPLE.ID", descriptor.getPrimaryKeyFieldNames().get(0));
        assertEquals(2, descriptor.getMappings().size());

        DirectToFieldMapping mapping = (DirectToFieldMapping) descriptor.getMappingForAttributeName("id");
        assertNotNull(mapping);
        assertEquals("DYNAMIC_SIMPLE.ID", mapping.getFieldName());
        assertEquals(Integer.class, mapping.getAttributeClassification());

        mapping = (DirectToFieldMapping) descriptor.getMappingForAttributeName("value");
        assertNotNull(mapping);
        assertEquals("DYNAMIC_SIMPLE.VALUE", mapping.getFieldName());
        assertEquals(String.class, mapping.getAttributeClassification());
    }

    @Test
    public void persistSimpleTypeInstances() {
        ClassDescriptor descriptor = getDescriptor("SimpleType");

        Map newEntity = example.persistDynamicInstances(getSession(), descriptor);

        assertNotNull(newEntity);

        Session session = getSession();

        ReportQuery rq = new ReportQuery(descriptor.getJavaClass(), new ExpressionBuilder());
        rq.addCount();
        rq.setShouldReturnSingleValue(true);
        int count = ((Number) session.executeQuery(rq)).intValue();
        assertEquals(1, count);
    }

    @Test
    public void querySimpleTypeInstances() {
        ClassDescriptor descriptor = getDescriptor("SimpleType");

        List<Map> entities = example.queryDynamicInstances(getSession(), descriptor);

        assertNotNull(entities);
        assertEquals(1, entities.size());
        assertEquals(1, entities.get(0).get("id"));
        assertEquals("value-1", entities.get(0).get("value"));
    }

    @Test
    public void updateSimpleTypeInstances() {
        ClassDescriptor descriptor = getDescriptor("SimpleType");

        Map entity = example.updateDyanmicInstances(getSession(), descriptor);

        assertNotNull(entity);
        assertEquals(1, entity.get("id"));
        assertEquals("value-1+", entity.get("value"));
    }

    @Test
    public void deleteSimpleTypeInstances() {
        ClassDescriptor descriptor = getDescriptor("SimpleType");

        example.deleteDynamicInstances(getSession(), descriptor);

        Session session = getSession();

        ReportQuery rq = new ReportQuery(descriptor.getJavaClass(), new ExpressionBuilder());
        rq.addCount();
        rq.setShouldReturnSingleValue(true);
        int count = ((Number) session.executeQuery(rq)).intValue();
        assertEquals(0, count);
    }

    @Test
    public void removeSimpleType() {
        ClassDescriptor descriptor = getDescriptor("SimpleType");

        example.removeDynamicType(getSession(), descriptor);

        descriptor = getSession().getDescriptorForAlias("SimpleType");
        assertNull(descriptor);
        assertEquals(0, getSession().getDescriptors().size());
    }

}
