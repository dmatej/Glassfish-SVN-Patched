package example;

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

import java.util.Collection;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.FlushModeType;

import org.eclipse.persistence.config.PessimisticLock;
import org.eclipse.persistence.config.QueryHints;
import org.eclipse.persistence.dynamic.DynamicEntity;
import org.eclipse.persistence.dynamic.DynamicHelper;
import org.eclipse.persistence.dynamic.DynamicType;
import org.eclipse.persistence.jpa.dynamic.JPADynamicHelper;

/**
 * 
 * @author dclarke
 * @since EclipseLink - Dynamic Incubator (1.1.0-branch)
 */
public class Transactions {

    /**
     * New entities with new related related entities can be persisted using
     * <code>EntityManager.persist(newEntity)</code>. The cascade setting on the
     * mappings determine how the related entities are handled. In this case
     * Employee has its relationship to Address and PhoneNumber configured with
     * cascade-all so the associated new entities will also be persisted.
     */
    public DynamicEntity createUsingPersist(EntityManager em) {
        DynamicHelper helper = new JPADynamicHelper(em);

        DynamicType empType = helper.getType("Employee");
        DynamicType addrType = helper.getType("Address");
        DynamicType phoneType = helper.getType("PhoneNumber");

        DynamicEntity emp = (DynamicEntity) empType.newDynamicEntity();
        emp.set("firstName", "Sample");
        emp.set("lastName", "Employee");
        emp.set("gender", "Male");
        emp.set("salary", 123456);

        DynamicEntity address = (DynamicEntity) addrType.newDynamicEntity();
        emp.set("address", address);

        DynamicEntity phone = (DynamicEntity) phoneType.newDynamicEntity();
        phone.set("type", "Mobile");
        phone.set("areaCode", "613");
        phone.set("number", "555-1212");
        phone.set("owner", emp);
        emp.<Collection<DynamicEntity>> get("phoneNumbers").add(phone);

        em.getTransaction().begin();
        em.persist(emp);
        em.getTransaction().commit();

        return emp;
    }

    /**
	 * 
	 */
    public DynamicEntity createUsingMerge(EntityManager em) {
        JPADynamicHelper helper = new JPADynamicHelper(em);

        DynamicEntity emp = helper.getType("Employee").newDynamicEntity();
        emp.set("firstName", "Sample");
        emp.set("lastName", "Employee");
        emp.set("gender", "Male");
        emp.set("salary", 123456);

        DynamicEntity address = helper.getType("Address").newDynamicEntity();
        emp.set("address", address);

        DynamicEntity phone = helper.getType("PhoneNumber").newDynamicEntity();
        phone.set("type", "Mobile");
        phone.set("areaCode", "613");
        phone.set("number", "555-1212");
        phone.set("owner", emp);
        emp.<Collection<DynamicEntity>> get("phoneNumbers").add(phone);

        em.getTransaction().begin();
        // When merging the managed instance is returned from the call.
        // Further usage within the transaction must be done with this managed
        // entity.
        emp = (DynamicEntity) em.merge(emp);
        em.getTransaction().commit();

        return emp;
    }

    /**
     * 
     * @param em
     * @return
     */
    public DynamicEntity createWithRelationshipsToExistingEntities(EntityManager em) {
        return null;
    }

    /**
     * 
     * @param em
     */
    public DynamicEntity deleteEntity(EntityManager em) {
        return null;
    }

    /**
     * Example of in-memory query against the transactional state without
     * flushing it to the database.
     * 
     * @param em
     */
    public void queriesOnTransactionalState(EntityManager em) {
        em.setFlushMode(FlushModeType.COMMIT);

    }

    /**
     * 
     * @param em
     * @throws Exception
     */
    public void pessimisticLocking(EntityManager em) throws Exception {

        // Find the Employee with the minimum ID
        int minId = new Queries().minimumEmployeeId(em);

        em.getTransaction().begin();

        // Lock Employee using query with hint
        DynamicEntity emp = (DynamicEntity) em.createQuery("SELECT e FROM Employee e WHERE e.id = :ID").setParameter("ID", minId).setHint(QueryHints.PESSIMISTIC_LOCK, PessimisticLock.Lock).getSingleResult();

        emp.set("salary", emp.<Integer> get("salary") - 1);

        em.flush();
    }

    /**
     * This example illustrates the use of a query returning an entity and data
     * from a related entity within a transaction. The returned entities are
     * managed and thus any changes are reflected in the database upon flush.
     * 
     * @param em
     * @throws Exception
     */
    @SuppressWarnings("unchecked")
    public void updateEmployeeWithCity(EntityManager em) throws Exception {
        em.getTransaction().begin();

        List<Object[]> emps = em.createQuery("SELECT e, e.address.city FROM Employee e").getResultList();
        DynamicEntity emp = (DynamicEntity) emps.get(0)[0];
        emp.set("salary", emp.<Integer> get("salary") + 1);

        em.flush();

        em.getTransaction().rollback();
    }

}
