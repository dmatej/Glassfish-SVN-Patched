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
package example.jpa;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import model.DynamicMapEntity;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.descriptors.RelationalDescriptor;
import org.eclipse.persistence.dynamic.DynamicClassLoader;
import org.eclipse.persistence.expressions.ExpressionBuilder;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.mappings.DirectToFieldMapping;
import org.eclipse.persistence.queries.ReadObjectQuery;
import org.eclipse.persistence.sessions.server.Server;
import org.eclipse.persistence.tools.schemaframework.SchemaManager;

import example.util.ExamplePropertiesLoader;

public class DynamicMapExample {

    /**
     * 
     */
    public static void main(String[] args) throws Exception {
        DynamicMapExample example = new DynamicMapExample();

        Map<String, Object> properties = new HashMap<String, Object>();
        ExamplePropertiesLoader.loadProperties(properties);
        EntityManagerFactory emf = Persistence.createEntityManagerFactory("dynamic-simple-map", properties);

        try {

            ClassDescriptor descriptor = example.createDynamicType(emf);

            example.persistDynamicInstances(emf, descriptor);
            example.queryDynamicInstances(emf, descriptor);
            example.updateDyanmicInstances(emf, descriptor);
            example.deleteDynamicInstances(emf, descriptor);

            example.removeDynamicType(emf, descriptor);

        } finally {
            emf.close();
        }
    }

    /**
     * Create a new dynamic type called 'SimpleType' with the class name of
     * 'model.SimpleType'. The generated class would look like: <code>
     * package model;
     * public class SimpleType extends example.dynamic.DynamicEntity {}
     * </code>
     * 
     * The attributes defined in the mapping give the class the apparent
     * structure of: <code>
     * package model;
     * public class SimpleType extends example.dynamic.DynamicEntity {
     *    Integer id;
     *    String value;
     * }
     * </code>
     */
    public ClassDescriptor createDynamicType(EntityManagerFactory emf) throws ClassNotFoundException {
        Server session = JpaHelper.getServerSession(emf);

        DynamicClassLoader dcl = DynamicClassLoader.lookup(session);
        dcl.addClass("model.SimpleType", DynamicMapEntity.class);

        RelationalDescriptor descriptor = new RelationalDescriptor();
        descriptor.setJavaClass(dcl.loadClass("model.SimpleType"));
        descriptor.setTableName("DYNAMIC_SIMPLE");
        descriptor.setPrimaryKeyFieldName("ID");

        DirectToFieldMapping mapping = (DirectToFieldMapping) descriptor.addDirectMapping("id", "ID");
        mapping.setAttributeAccessor(new DynamicMapEntity.ValueAccessor(mapping, Integer.class));
        mapping = (DirectToFieldMapping) descriptor.addDirectMapping("value", "VALUE");
        mapping.setAttributeAccessor(new DynamicMapEntity.ValueAccessor(mapping, String.class));

        session.addDescriptor(descriptor);

        // Create the underlying table on the database. Drop it if it already
        // exists
        new SchemaManager(session).replaceDefaultTables();

        return descriptor;
    }

    public Map persistDynamicInstances(EntityManagerFactory emf, ClassDescriptor descriptor) {
        EntityManager em = emf.createEntityManager();

        em.getTransaction().begin();

        Map entity = (Map) descriptor.getInstantiationPolicy().buildNewInstance();
        entity.put("id", 1);
        entity.put("value", "value-1");

        em.persist(entity);

        em.getTransaction().commit();
        em.close();

        return entity;
    }

    public List<Map> queryDynamicInstances(EntityManagerFactory emf, ClassDescriptor descriptor) {
        EntityManager em = emf.createEntityManager();

        try {
            return em.createQuery("SELECT s FROM SimpleType s WHERE s.value LIKE 'v%'").getResultList();
        } finally {
            em.close();
        }
    }

    public Map updateDyanmicInstances(EntityManagerFactory emf, ClassDescriptor descriptor) {
        EntityManager em = emf.createEntityManager();

        em.getTransaction().begin();

        ReadObjectQuery query = new ReadObjectQuery(descriptor.getJavaClass());
        ExpressionBuilder eb = query.getExpressionBuilder();
        query.setSelectionCriteria(eb.get("id").equal(1));
        Map entity = (Map) JpaHelper.createQuery(query, em).getSingleResult();

        entity.put("value", "value-1+");

        em.getTransaction().commit();
        em.close();

        return entity;
    }

    public void deleteDynamicInstances(EntityManagerFactory emf, ClassDescriptor descriptor) {
        EntityManager em = emf.createEntityManager();

        em.getTransaction().begin();

        ReadObjectQuery query = new ReadObjectQuery(descriptor.getJavaClass());
        ExpressionBuilder eb = query.getExpressionBuilder();
        query.setSelectionCriteria(eb.get("id").equal(1));
        Map entity = (Map) JpaHelper.createQuery(query, em).getSingleResult();

        em.remove(entity);

        em.getTransaction().commit();
        em.close();
    }

    public void removeDynamicType(EntityManagerFactory emf, ClassDescriptor descriptor) {
        Server session = JpaHelper.getServerSession(emf);

        session.getIdentityMapAccessor().initializeIdentityMap(descriptor.getJavaClass());

        session.getDescriptors().remove(descriptor.getJavaClass());
        session.getProject().getAliasDescriptors().remove(descriptor.getAlias());
        session.getProject().getOrderedDescriptors().remove(descriptor);

    }

}
