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
 *     dclarke - Dynamic Persistence INCUBATION - Enhancement 200045
 *               http://wiki.eclipse.org/EclipseLink/Development/JPA/Dynamic
 *     
 * This code is being developed under INCUBATION and is not currently included 
 * in the automated EclipseLink build. The API in this code may change, or 
 * may never be included in the product. Please provide feedback through mailing 
 * lists or the bug database.
 ******************************************************************************/
package example;

import java.util.Collection;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.Query;

import org.eclipse.persistence.config.QueryHints;
import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.dynamic.DynamicEntity;
import org.eclipse.persistence.dynamic.DynamicType;
import org.eclipse.persistence.expressions.ExpressionBuilder;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.queries.ReadAllQuery;

/**
 * Simple query examples for the XML mapped Employee domain model.
 * 
 * @author dclarke
 * @since EclipseLink - Dynamic Incubator (1.1.0-branch)
 */
public class Queries {
    
    public DynamicEntity findEmployee(EntityManager em, DynamicType type, Object id) {
        return (DynamicEntity) em.find(type.getJavaClass(), id);
    }
    
    /**
     * Simple example using dynamic JP QL to retrieve all Employee instances
     * sorted by lastName and firstName.
     */
    @SuppressWarnings("unchecked")
    public List<DynamicEntity> readAllEmployeesUsing(EntityManager em) {
        return em.createQuery("SELECT e FROM Employee e ORDER BY e.id ASC").getResultList();
    }

    @SuppressWarnings("unchecked")
    public List<DynamicEntity> joinFetchEmployeeWithAddress(EntityManager em) {
        return em.createQuery("SELECT e FROM Employee e JOIN FETCH e.address ORDER BY e.lastName ASC, e.firstName ASC").getResultList();
    }

    @SuppressWarnings("unchecked")
    public List<DynamicEntity> joinFetchHint(EntityManager em) {
        Query query = em.createQuery("SELECT e FROM Employee e WHERE e.manager.address.city = 'Ottawa' ORDER BY e.lastName ASC, e.firstName ASC");
        query.setHint(QueryHints.FETCH, "e.address");
        query.setHint(QueryHints.FETCH, "e.manager");
        query.setHint(QueryHints.FETCH, "e.manager.address");
        query.setHint(QueryHints.BATCH, "e.manager.phoneNumbers");
        List<DynamicEntity> emps = query.getResultList();

        for (DynamicEntity emp : emps) {
            emp.<DynamicEntity>get("manager").<Collection>get("phoneNumbers").size();
        }

        return emps;
    }

    public int minimumEmployeeId(EntityManager em) {
        return ((Number) em.createQuery("SELECT MIN(e.id) FROM Employee e").getSingleResult()).intValue();
    }

    public DynamicEntity minimumEmployee(EntityManager em) {
        Query q = em.createQuery("SELECT e FROM Employee e WHERE e.id in (SELECT MIN(ee.id) FROM Employee ee)");

        return (DynamicEntity) q.getSingleResult();
    }

    @SuppressWarnings("unchecked")
    public List<DynamicEntity> findEmployeesUsingGenderIn(EntityManager em) {
        return em.createQuery("SELECT e FROM Employee e WHERE e.gender IN (:GENDER1, :GENDER2)").setParameter("GENDER1", "Male").setParameter("GENDER2", "Female").getResultList();
    }

    @SuppressWarnings("unchecked")
    public List<DynamicEntity> findUsingNativeReadAllQuery(EntityManager em) {
        ClassDescriptor descriptor = JpaHelper.getEntityManager(em).getServerSession().getDescriptorForAlias("Employee");
        ReadAllQuery raq = new ReadAllQuery(descriptor.getJavaClass());
        ExpressionBuilder eb = raq.getExpressionBuilder();
        raq.setSelectionCriteria(eb.get("gender").equal("Male"));

        Query query = JpaHelper.createQuery(raq, em);

        return query.getResultList();
    }

    public DynamicEntity minEmployeeWithAddressAndPhones(EntityManager em) {
        return (DynamicEntity) em.createQuery("SELECT e FROM Employee e JOIN FETCH e.address WHERE e.id IN (SELECT MIN(p.id) FROM PhoneNumber p)").getSingleResult();
    }
}
