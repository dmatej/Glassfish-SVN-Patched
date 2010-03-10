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
package example.nativeorm;

import java.util.List;
import java.util.Map;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.expressions.ExpressionBuilder;
import org.eclipse.persistence.queries.ReadAllQuery;
import org.eclipse.persistence.queries.ReadObjectQuery;
import org.eclipse.persistence.sessions.DatabaseSession;
import org.eclipse.persistence.sessions.UnitOfWork;

public class SimpleDynamicMap_NativeExample {

    /**
     * 
     */
    public static void main(String[] args) {
        SimpleDynamicMap_NativeExample example = new SimpleDynamicMap_NativeExample();

        DatabaseSession session = ExampleHelper.createSession();
        ClassDescriptor descriptor = ExampleHelper.createDynamicType(session);

        example.persistDynamicInstances(session, descriptor);
        example.queryDynamicInstances(session, descriptor);
        example.updateDyanmicInstances(session, descriptor);
        example.deleteDynamicInstances(session, descriptor);

        example.removeDynamicType(session, descriptor);

        try {

        } finally {
            session.logout();
        }
    }

    public Map persistDynamicInstances(DatabaseSession session, ClassDescriptor descriptor) {
        UnitOfWork uow = session.acquireUnitOfWork();

        Map entity = (Map) uow.newInstance(descriptor.getJavaClass());
        entity.put("id", 1);
        entity.put("value", "value-1");

        uow.commit();

        return entity;
    }

    public List<Map> queryDynamicInstances(DatabaseSession session, ClassDescriptor descriptor) {
        ReadAllQuery query = new ReadAllQuery(descriptor.getJavaClass());
        ExpressionBuilder eb = query.getExpressionBuilder();
        query.setSelectionCriteria(eb.get("value").like("v%"));

        return (List<Map>) session.executeQuery(query);
    }

    public Map updateDyanmicInstances(DatabaseSession session, ClassDescriptor descriptor) {
        UnitOfWork uow = session.acquireUnitOfWork();

        ReadObjectQuery query = new ReadObjectQuery(descriptor.getJavaClass());
        ExpressionBuilder eb = query.getExpressionBuilder();
        query.setSelectionCriteria(eb.get("id").equal(1));

        Map entity = (Map) uow.executeQuery(query);

        entity.put("value", "value-1+");

        uow.commit();

        return entity;
    }

    public void deleteDynamicInstances(DatabaseSession session, ClassDescriptor descriptor) {
        UnitOfWork uow = session.acquireUnitOfWork();

        ReadObjectQuery query = new ReadObjectQuery(descriptor.getJavaClass());
        ExpressionBuilder eb = query.getExpressionBuilder();
        query.setSelectionCriteria(eb.get("id").equal(1));

        Map entity = (Map) uow.executeQuery(query);

        uow.deleteObject(entity);

        uow.commit();
    }

    public void removeDynamicType(DatabaseSession session, ClassDescriptor descriptor) {
        session.getIdentityMapAccessor().initializeIdentityMap(descriptor.getJavaClass());

        session.getDescriptors().remove(descriptor.getJavaClass());
        session.getProject().getAliasDescriptors().remove(descriptor.getAlias());
        session.getProject().getOrderedDescriptors().remove(descriptor);

    }

}
