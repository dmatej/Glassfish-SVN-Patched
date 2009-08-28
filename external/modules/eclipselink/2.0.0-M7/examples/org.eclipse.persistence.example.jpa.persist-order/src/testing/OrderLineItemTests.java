/*******************************************************************************
 * Copyright (c) 2008 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     dclarke - Initial persist-order example (bug 218321) 
 ******************************************************************************/
package testing;

import static org.junit.Assert.*;
import static testing.CreateDatabase.assertOrderMatchesDatabase;

import java.beans.PropertyChangeEvent;
import java.io.IOException;
import java.io.Serializable;
import java.util.*;

import javax.persistence.*;

import model.*;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.descriptors.changetracking.AttributeChangeTrackingPolicy;
import org.eclipse.persistence.descriptors.changetracking.ChangeTracker;
import org.eclipse.persistence.indirection.IndirectContainer;
import org.eclipse.persistence.internal.descriptors.PersistenceEntity;
import org.eclipse.persistence.internal.helper.SerializationHelper;
import org.eclipse.persistence.internal.sessions.UnitOfWorkImpl;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.mappings.DirectToFieldMapping;
import org.eclipse.persistence.mappings.OneToManyMapping;
import org.eclipse.persistence.sessions.IdentityMapAccessor;
import org.junit.*;

/*******************************************************************************
 * Copyright (c) 1998, 2009 Oracle. All rights reserved. This program and the
 * accompanying materials are made available under the terms of the Eclipse
 * Public License v1.0 and Eclipse Distribution License v. 1.0 which accompanies
 * this distribution. The Eclipse Public License is available at
 * http://www.eclipse.org/legal/epl-v10.html and the Eclipse Distribution
 * License is available at http://www.eclipse.org/org/documents/edl-v10.php.
 * 
 * Contributors: dclarke - Example: Maintaining Collection Order (Bug 218321)
 * http://wiki.eclipse.org/EclipseLink/Examples/JPA/Collectionordering
 * 
 *******************************************************************************/
public class OrderLineItemTests {

    @Test
    public void insertNewOrderWithNoItemsUsingPersist() {
        EntityManager em = getEMF().createEntityManager();

        Order order = new Order();
        order.setOrderNumber("ORDER-3");

        verifyLineItems(order, 0, new int[0], new int[0], "BEFORE PERSIST");

        em.getTransaction().begin();
        em.persist(order);

        em.getTransaction().commit();

        if (!orderEntityIsolated(em)) {
            order = getOrderFromCache(em, order.getOrderNumber());
            verifyLineItems(order, 0, new int[0], new int[0], "AFTER COMMIT");
        }
        assertOrderMatchesDatabase(getEMF(), order);

        em.close();
    }

    @Test
    public void insertNewOrderWithNoItemsUsingMerge() {
        EntityManager em = getEMF().createEntityManager();

        Order order = new Order();
        order.setOrderNumber("ORDER-3");

        verifyLineItems(order, 0, new int[0], new int[0], "BEFORE PERSIST");

        em.getTransaction().begin();
        em.merge(order);

        em.getTransaction().commit();

        if (!orderEntityIsolated(em)) {
            order = getOrderFromCache(em, order.getOrderNumber());
            verifyLineItems(order, 0, new int[0], new int[0], "AFTER COMMIT");
        }
        assertOrderMatchesDatabase(getEMF(), order);

        em.close();
    }

    @Test
    public void insertNewOrderWithItemsUsingPersist() {
        EntityManager em = getEMF().createEntityManager();
        em.getTransaction().begin();

        Order order = new Order();
        order.setOrderNumber("ORDER-2");

        verifyLineItems(order, 0, new int[0], new int[0], "BEFORE ADD LINE ITEMS");

        Product prod1 = em.find(Product.class, 1);
        Product prod2 = em.find(Product.class, 2);
        Product prod3 = em.find(Product.class, 3);

        order.addLineItem(new LineItem(prod1, 2, 222d));
        order.addLineItem(0, new LineItem(prod2, 1, 100d));
        order.addLineItem(new LineItem(prod3, 3, 3.50d));

        verifyLineItems(order, 3, new int[] { -1, -1, -1 }, new int[] { 2, 1, 3 }, "BEFORE PERSIST");

        em.persist(order);

        verifyLineItems(order, 3, new int[] { -1, -1, -1 }, new int[] { 2, 1, 3 }, "AFTER PERSIST");

        em.getTransaction().commit();

        order = em.find(Order.class, order.getOrderNumber());
        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 2, 1, 3 }, "AFTER COMMIT (working copy)");

        if (!orderEntityIsolated(em)) {
            order = getOrderFromCache(em, order.getOrderNumber());
            verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 2, 1, 3 }, "AFTER COMMIT (shared cache)");
        }
        assertOrderMatchesDatabase(getEMF(), order);

        em.close();
    }

    @Test
    public void insertNewOrderWithItemsUsingMerge() {
        EntityManager em = getEMF().createEntityManager();
        em.getTransaction().begin();

        Order order = new Order();
        order.setOrderNumber("ORDER-2");

        verifyLineItems(order, 0, new int[0], new int[0], "BEFORE ADD LINE ITEMS");

        Product prod1 = em.find(Product.class, 1);
        Product prod2 = em.find(Product.class, 2);
        Product prod3 = em.find(Product.class, 3);

        order.addLineItem(new LineItem(prod1, 2, 222d));
        order.addLineItem(0, new LineItem(prod2, 1, 100d));
        order.addLineItem(new LineItem(prod3, 3, 3.50d));

        verifyLineItems(order, 3, new int[] { -1, -1, -1 }, new int[] { 2, 1, 3 }, "BEFORE MERGE");

        Order orderWC = em.merge(order);

        verifyLineItems(order, 3, new int[] { -1, -1, -1 }, new int[] { 2, 1, 3 }, "AFTER MERGE (merge arg)");
        verifyLineItems(orderWC, 3, new int[] { -1, -1, -1 }, new int[] { 2, 1, 3 }, "AFTER MERGE (merge return)");

        em.getTransaction().commit();

        order = em.find(Order.class, order.getOrderNumber());
        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 2, 1, 3 }, "AFTER COMMIT (working copy)");

        if (!orderEntityIsolated(em)) {
            order = getOrderFromCache(em, order.getOrderNumber());
            verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 2, 1, 3 }, "AFTER COMMIT (shared cache)");
        }
        assertOrderMatchesDatabase(getEMF(), order);

        em.close();
    }

    @Test
    public void addNewLineItem() {
        EntityManager em = getEMF().createEntityManager();

        em.getTransaction().begin();

        Product product = em.find(Product.class, 4);
        Order order = em.find(Order.class, "ORDER-1");
        assertOrderMatchesDatabase(getEMF(), order);

        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "BEFORE ADD");

        order.addLineItem(new LineItem(product, 99, 99.99));

        verifyLineItems(order, 4, new int[] { 0, 1, 2, -1 }, new int[] { 1, 2, 3, 4 }, "AFTER ADD");

        em.getTransaction().commit();

        order = em.find(Order.class, order.getOrderNumber());
        verifyLineItems(order, 4, new int[] { 0, 1, 2, 3 }, new int[] { 1, 2, 3, 4 }, "AFTER COMMIT (working copy)");

        if (!orderEntityIsolated(em)) {
            order = getOrderFromCache(em, order.getOrderNumber());
            verifyLineItems(order, 4, new int[] { 0, 1, 2, 3 }, new int[] { 1, 2, 3, 4 }, "AFTER COMMIT (shared cache)");
        }
        assertOrderMatchesDatabase(getEMF(), order);

        em.close();
    }

    @Test
    public void addNewLineItemFirst() {
        EntityManager em = getEMF().createEntityManager();

        em.getTransaction().begin();

        Product product = em.find(Product.class, 4);
        Order order = em.find(Order.class, "ORDER-1");
        assertOrderMatchesDatabase(getEMF(), order);

        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "BEFORE ADD");

        order.addLineItem(0, new LineItem(product, 99, 99.99));

        verifyLineItems(order, 4, new int[] { -1, 0, 1, 2 }, new int[] { 4, 1, 2, 3 }, "AFTER ADD");

        em.getTransaction().commit();

        order = em.find(Order.class, order.getOrderNumber());
        verifyLineItems(order, 4, new int[] { 0, 1, 2, 3 }, new int[] { 4, 1, 2, 3 }, "AFTER COMMIT (working copy)");

        if (!orderEntityIsolated(em)) {
            order = getOrderFromCache(em, order.getOrderNumber());
            verifyLineItems(order, 4, new int[] { 0, 1, 2, 3 }, new int[] { 4, 1, 2, 3 }, "AFTER COMMIT (shared cache)");
        }
        assertOrderMatchesDatabase(getEMF(), order);

        em.close();
    }

    @Test
    public void addNewLineItemSecond() {
        EntityManager em = getEMF().createEntityManager();

        em.getTransaction().begin();

        Product product = em.find(Product.class, 4);
        Order order = em.find(Order.class, "ORDER-1");
        assertOrderMatchesDatabase(getEMF(), order);

        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "BEFORE ADD");

        order.addLineItem(1, new LineItem(product, 99, 99.99));

        verifyLineItems(order, 4, new int[] { 0, -1, 1, 2 }, new int[] { 1, 4, 2, 3 }, "AFTER ADD");

        em.getTransaction().commit();

        order = em.find(Order.class, order.getOrderNumber());
        verifyLineItems(order, 4, new int[] { 0, 1, 2, 3 }, new int[] { 1, 4, 2, 3 }, "AFTER COMMIT (working copy)");

        if (!orderEntityIsolated(em)) {
            order = getOrderFromCache(em, order.getOrderNumber());
            verifyLineItems(order, 4, new int[] { 0, 1, 2, 3 }, new int[] { 1, 4, 2, 3 }, "AFTER COMMIT (shared cache)");
        }
        assertOrderMatchesDatabase(getEMF(), order);

        em.close();
    }

    @Test
    public void addNewLineItemThird() {
        EntityManager em = getEMF().createEntityManager();

        em.getTransaction().begin();

        Product product = em.find(Product.class, 4);
        Order order = em.find(Order.class, "ORDER-1");
        assertOrderMatchesDatabase(getEMF(), order);

        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "BEFORE ADD");

        order.addLineItem(2, new LineItem(product, 99, 99.99));

        verifyLineItems(order, 4, new int[] { 0, 1, -1, 2 }, new int[] { 1, 2, 4, 3 }, "AFTER ADD");

        em.getTransaction().commit();

        order = em.find(Order.class, order.getOrderNumber());
        verifyLineItems(order, 4, new int[] { 0, 1, 2, 3 }, new int[] { 1, 2, 4, 3 }, "AFTER COMMIT (working copy)");

        if (!orderEntityIsolated(em)) {
            order = getOrderFromCache(em, order.getOrderNumber());
            verifyLineItems(order, 4, new int[] { 0, 1, 2, 3 }, new int[] { 1, 2, 4, 3 }, "AFTER COMMIT (shared cache)");
        }
        assertOrderMatchesDatabase(getEMF(), order);

        em.close();
    }

    @Test
    public void removeFirstLineItem() {
        EntityManager em = getEMF().createEntityManager();

        em.getTransaction().begin();

        Order order = em.find(Order.class, "ORDER-1");

        assertOrderMatchesDatabase(getEMF(), order);
        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "BEFORE REMOVE");

        LineItem item = order.getLineItems().remove(0);
        em.remove(item);

        verifyLineItems(order, 2, new int[] { 1, 2 }, new int[] { 2, 3 }, "AFTER REMOVE");

        em.getTransaction().commit();

        order = em.find(Order.class, order.getOrderNumber());
        verifyLineItems(order, 2, new int[] { 0, 1 }, new int[] { 2, 3 }, "AFTER COMMIT (working copy)");

        if (!orderEntityIsolated(em)) {
            order = getOrderFromCache(em, order.getOrderNumber());
            verifyLineItems(order, 2, new int[] { 0, 1 }, new int[] { 2, 3 }, "AFTER COMMIT (shared cache)");
        }
        assertOrderMatchesDatabase(getEMF(), order);

        em.close();
    }

    @Test
    public void reorderItems_Move0to1() {
        EntityManager em = getEMF().createEntityManager();

        em.getTransaction().begin();

        Order order = em.find(Order.class, "ORDER-1");

        assertOrderMatchesDatabase(getEMF(), order);
        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "BEFORE MOVE");

        LineItem item = order.getLineItems().remove(0);
        order.addLineItem(1, item);

        verifyLineItems(order, 3, new int[] { 1, 0, 2 }, new int[] { 2, 1, 3 }, "AFTER MOVE");

        em.getTransaction().commit();

        order = em.find(Order.class, order.getOrderNumber());
        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 2, 1, 3 }, "AFTER COMMIT (working copy)");

        if (!orderEntityIsolated(em)) {
            order = getOrderFromCache(em, order.getOrderNumber());
            verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 2, 1, 3 }, "AFTER COMMIT (shared cache)");
        }
        assertOrderMatchesDatabase(getEMF(), order);

        em.close();
    }

    @Test
    public void reorderItems_Move1to0() {
        EntityManager em = getEMF().createEntityManager();

        em.getTransaction().begin();

        Order order = em.find(Order.class, "ORDER-1");

        assertOrderMatchesDatabase(getEMF(), order);
        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "BEFORE MOVE");

        LineItem item = order.getLineItems().remove(1);
        order.addLineItem(0, item);

        verifyLineItems(order, 3, new int[] { 1, 0, 2 }, new int[] { 2, 1, 3 }, "AFTER MOVE");

        em.getTransaction().commit();

        order = em.find(Order.class, order.getOrderNumber());
        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 2, 1, 3 }, "AFTER COMMIT (working copy)");

        if (!orderEntityIsolated(em)) {
            order = getOrderFromCache(em, order.getOrderNumber());
            verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 2, 1, 3 }, "AFTER COMMIT (shared cache)");
        }
        assertOrderMatchesDatabase(getEMF(), order);

        em.close();
    }

    @Test
    public void reorderItems_Move2to1() {
        EntityManager em = getEMF().createEntityManager();

        em.getTransaction().begin();

        Order order = em.find(Order.class, "ORDER-1");

        assertOrderMatchesDatabase(getEMF(), order);
        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "BEFORE MOVE");

        LineItem item = order.getLineItems().remove(2);
        order.addLineItem(1, item);

        verifyLineItems(order, 3, new int[] { 0, 2, 1 }, new int[] { 1, 3, 2 }, "AFTER MOVE");

        em.getTransaction().commit();

        order = em.find(Order.class, order.getOrderNumber());
        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 3, 2 }, "AFTER COMMIT (working copy)");

        if (!orderEntityIsolated(em)) {
            order = getOrderFromCache(em, order.getOrderNumber());
            verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 3, 2 }, "AFTER COMMIT (shared cache)");
        }
        assertOrderMatchesDatabase(getEMF(), order);

        em.close();
    }

    @Test
    public void reorderItems_Move0to1_Detached() throws Exception {
        EntityManager em = getEMF().createEntityManager();

        em.getTransaction().begin();

        Order order = em.find(Order.class, "ORDER-1");

        assertOrderMatchesDatabase(getEMF(), order);
        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "BEFORE MOVE (managed)");

        order = (Order) serialize(order);

        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "BEFORE MOVE (detached)");

        LineItem item = order.getLineItems().remove(0);
        order.addLineItem(1, item);

        verifyLineItems(order, 3, new int[] { 1, 0, 2 }, new int[] { 2, 1, 3 }, "AFTER MOVE (detached)");

        Order orderWC = em.merge(order);

        verifyLineItems(order, 3, new int[] { 1, 0, 2 }, new int[] { 2, 1, 3 }, "AFTER MERGE (detached)");
        verifyLineItems(orderWC, 3, new int[] { 1, 0, 2 }, new int[] { 2, 1, 3 }, "AFTER MERGE (managed)");

        em.getTransaction().commit();

        order = em.find(Order.class, order.getOrderNumber());
        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 2, 1, 3 }, "AFTER COMMIT (working copy)");

        if (!orderEntityIsolated(em)) {
            order = getOrderFromCache(em, order.getOrderNumber());
            verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 2, 1, 3 }, "AFTER COMMIT (shared cache)");
        }
        assertOrderMatchesDatabase(getEMF(), order);

        em.close();
    }

    @Test
    public void reorderItemsAndAddOne() {
        EntityManager em = getEMF().createEntityManager();

        em.getTransaction().begin();

        Product product = em.find(Product.class, 4);
        Order order = em.find(Order.class, "ORDER-1");

        assertOrderMatchesDatabase(getEMF(), order);
        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "BEFORE MOVE");

        LineItem item = order.getLineItems().remove(0);
        order.addLineItem(1, item);

        verifyLineItems(order, 3, new int[] { 1, 0, 2 }, new int[] { 2, 1, 3 }, "AFTER MOVE, BEFORE ADD");

        order.addLineItem(new LineItem(product, 99, 99.99));

        verifyLineItems(order, 4, new int[] { 1, 0, 2, -1 }, new int[] { 2, 1, 3, 4 }, "AFTER MOVE AND ADD");

        em.getTransaction().commit();

        order = em.find(Order.class, order.getOrderNumber());
        verifyLineItems(order, 4, new int[] { 0, 1, 2, 3 }, new int[] { 2, 1, 3, 4 }, "AFTER COMMIT (working copy)");

        if (!orderEntityIsolated(em)) {
            order = getOrderFromCache(em, order.getOrderNumber());
            verifyLineItems(order, 4, new int[] { 0, 1, 2, 3 }, new int[] { 2, 1, 3, 4 }, "AFTER COMMIT (shared cache)");
        }
        assertOrderMatchesDatabase(getEMF(), order);

        em.close();
    }

    @Test
    public void reorderItemsAndRemoveOne() {
        EntityManager em = getEMF().createEntityManager();

        em.getTransaction().begin();

        Order order = em.find(Order.class, "ORDER-1");

        assertOrderMatchesDatabase(getEMF(), order);
        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "BEFORE MOVE");

        LineItem item = order.getLineItems().remove(0);
        order.addLineItem(1, item);

        verifyLineItems(order, 3, new int[] { 1, 0, 2 }, new int[] { 2, 1, 3 }, "AFTER MOVE, BEFORE REMOVE");

        order.getLineItems().remove(2);

        verifyLineItems(order, 2, new int[] { 1, 0 }, new int[] { 2, 1 }, "AFTER MOVE AND REMOVE");

        em.getTransaction().commit();

        order = em.find(Order.class, order.getOrderNumber());
        verifyLineItems(order, 2, new int[] { 0, 1 }, new int[] { 2, 1 }, "AFTER COMMIT (working copy)");

        if (!orderEntityIsolated(em)) {
            order = getOrderFromCache(em, order.getOrderNumber());
            verifyLineItems(order, 2, new int[] { 0, 1 }, new int[] { 2, 1 }, "AFTER COMMIT (shared cache)");
        }
        assertOrderMatchesDatabase(getEMF(), order);

        em.close();
    }

    @Test
    public void removeLastLineItem() {
        EntityManager em = getEMF().createEntityManager();

        em.getTransaction().begin();

        Order order = em.find(Order.class, "ORDER-1");

        assertOrderMatchesDatabase(getEMF(), order);
        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "BEFORE REMOVE");

        LineItem item = order.getLineItems().remove(2);
        em.remove(item);

        verifyLineItems(order, 2, new int[] { 0, 1 }, new int[] { 1, 2 }, "BEFORE REMOVE");

        em.getTransaction().commit();

        order = em.find(Order.class, order.getOrderNumber());
        verifyLineItems(order, 2, new int[] { 0, 1 }, new int[] { 1, 2 }, "AFTER COMMIT (working copy)");

        if (!orderEntityIsolated(em)) {
            order = getOrderFromCache(em, order.getOrderNumber());
            verifyLineItems(order, 2, new int[] { 0, 1 }, new int[] { 1, 2 }, "AFTER COMMIT (shared cache)");
        }
        assertOrderMatchesDatabase(getEMF(), order);

        em.close();
    }

    @Test
    /*
     * Test updating the list by removing he first element.
     */
    public void clearLineItems() {
        EntityManager em = getEMF().createEntityManager();

        em.getTransaction().begin();

        Order order = em.find(Order.class, "ORDER-1");

        assertOrderMatchesDatabase(getEMF(), order);
        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "BEFORE CLEAR");

        order.getLineItems().clear();

        verifyLineItems(order, 0, new int[0], new int[0], "AFTER CLEAR");

        em.getTransaction().commit();

        order = em.find(Order.class, order.getOrderNumber());
        verifyLineItems(order, 0, new int[0], new int[0], "AFTER COMMIT (working copy)");

        if (!orderEntityIsolated(em)) {
            order = getOrderFromCache(em, order.getOrderNumber());
            verifyLineItems(order, 0, new int[0], new int[0], "AFTER COMMIT (shared cache)");
        }
        assertOrderMatchesDatabase(getEMF(), order);

        em.close();
    }

    @Test
    public void noChanges() {
        EntityManager em = getEMF().createEntityManager();

        em.getTransaction().begin();

        Order order = em.find(Order.class, "ORDER-1");
        assertNotNull(order);

        em.getTransaction().commit();

        em.close();
    }

    @Test
    public void noLineItemChanges() {
        EntityManager em = getEMF().createEntityManager();

        em.getTransaction().begin();

        Order order = em.find(Order.class, "ORDER-1");

        assertOrderMatchesDatabase(getEMF(), order);
        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "INITIAL FIND");

        order.setDescription("TEST");

        em.getTransaction().commit();

        order = em.find(Order.class, order.getOrderNumber());
        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "AFTER COMMIT (working copy)");

        if (!orderEntityIsolated(em)) {
            order = getOrderFromCache(em, order.getOrderNumber());
            verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "AFTER COMMIT (shared cache)");
        }
        assertOrderMatchesDatabase(getEMF(), order);

        em.close();
    }

    @Test
    public void resetLineNumbersWithoutMoving() {
        EntityManager em = getEMF().createEntityManager();

        em.getTransaction().begin();

        Order order = em.find(Order.class, "ORDER-1");

        assertNotNull(order);
        assertOrderMatchesDatabase(getEMF(), order);
        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "INITIAL FIND");

        DirectToFieldMapping lineNumMapping = (DirectToFieldMapping) JpaHelper.getServerSession(getEMF()).getClassDescriptorForAlias("LineItem").getMappingForAttributeName("lineNumber");
        for (LineItem li : order.getLineItems()) {
            Object originalValue = lineNumMapping.getAttributeValueFromObject(li);
            lineNumMapping.setAttributeValueInObject(li, -1);

            if (requiresWeaving()) {
                ((ChangeTracker) li)._persistence_getPropertyChangeListener().propertyChange(new PropertyChangeEvent(li, lineNumMapping.getAttributeName(), originalValue, -1));
            }
        }

        verifyLineItems(order, 3, new int[] { -1, -1, -1 }, new int[] { 1, 2, 3 }, "INITIAL FIND");

        em.getTransaction().commit();

        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "INITIAL FIND");

        em.close();
    }

    @Test
    public void deleteOrderUnmodified() {
        EntityManager em = getEMF().createEntityManager();

        em.getTransaction().begin();

        Order order = em.find(Order.class, "ORDER-1");

        assertOrderMatchesDatabase(getEMF(), order);
        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "INITIAL FIND");

        em.remove(order);

        em.getTransaction().commit();

        // Make sure that the order is not changed when removed
        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "POST COMMIT");

        order = em.find(Order.class, order.getOrderNumber());
        assertNull(order);

        em.close();
    }

    @Test
    public void deleteOrderWithReorderedItems() {
        EntityManager em = getEMF().createEntityManager();

        em.getTransaction().begin();

        Order order = em.find(Order.class, "ORDER-1");

        assertOrderMatchesDatabase(getEMF(), order);
        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "INITIAL FIND");

        // Modify the order's items
        LineItem li1 = order.getLineItems().remove(0);
        order.getLineItems().add(li1);

        verifyLineItems(order, 3, new int[] { 1, 2, 0 }, new int[] { 2, 3, 1 }, "POST MOVE 0->2");

        em.remove(order);

        em.getTransaction().commit();

        // Make sure that the order is not changed when removed
        verifyLineItems(order, 3, new int[] { 1, 2, 0 }, new int[] { 2, 3, 1 }, "POST MOVE 0->2");

        order = em.find(Order.class, order.getOrderNumber());
        assertNull(order);

        em.close();
    }

    @Test
    public void updateOrderWithExistingItemsUsingPersist() {
        EntityManager em = getEMF().createEntityManager();
        em.getTransaction().begin();
        Order order = new Order();
        order.setOrderNumber("ORDER-2");
        Product prod1 = em.find(Product.class, 1);
        order.addLineItem(new LineItem(prod1, 2, 222d));
        // verifyLineItems(order, 1, new int[] { -1 }, new int[] { 1 },
        // "BEFORE COMMIT");

        em.persist(order);
        // verifyLineItems(order, 1, new int[] { -1 }, new int[] { 1 },
        // "AFTER COMMIT First TX");

        em.getTransaction().commit();
        em.clear();

        em.getTransaction().begin();
        order = em.find(Order.class, order.getOrderNumber());
        // verifyLineItems(order, 1, new int[] { 0 }, new int[] { 1 },
        // "AFTER COMMIT (working copy) New TX");

        Product prod2 = em.find(Product.class, 2);
        Product prod3 = em.find(Product.class, 3);
        order.addLineItem(new LineItem(prod2, 1, 100d));
        order.addLineItem(new LineItem(prod3, 3, 3.50d));

        // TODO: comment in this line and the test will pass
        // verifyLineItems(order, 3, new int[] { 0, -1, -1 }, new int[] { 1, 2,3
        // }, "AFTER PERSIST");
        em.getTransaction().commit();
        order = em.find(Order.class, order.getOrderNumber());
        verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "AFTER COMMIT (working copy)");
        if (!orderEntityIsolated(em)) {
            order = getOrderFromCache(em, order.getOrderNumber());
            verifyLineItems(order, 3, new int[] { 0, 1, 2 }, new int[] { 1, 2, 3 }, "AFTER COMMIT (shared cache)");
        }
        assertOrderMatchesDatabase(getEMF(), order);
        em.close();
    }

    @Test
    public void updateOrderWithEmptyItemsUsingPersist() {
        EntityManager em = getEMF().createEntityManager();
        em.getTransaction().begin();
        Order order = new Order();
        order.setOrderNumber("ORDER-2");

        // verifyLineItems(order, 1, new int[] { -1 }, new int[] { 1 },
        // "BEFORE COMMIT");

        em.persist(order);
        // verifyLineItems(order, 1, new int[] { -1 }, new int[] { 1 },
        // "AFTER COMMIT First TX");

        em.getTransaction().commit();
        em.clear();

        em.getTransaction().begin();
        order = em.find(Order.class, order.getOrderNumber());
        // verifyLineItems(order, 1, new int[] { 0 }, new int[] { 1 },
        // "AFTER COMMIT (working copy) New TX");

        Product prod2 = em.find(Product.class, 2);
        Product prod3 = em.find(Product.class, 3);
        order.addLineItem(new LineItem(prod2, 1, 100d));
        order.addLineItem(new LineItem(prod3, 3, 3.50d));

        // TODO: comment in this line and the test will pass
        // verifyLineItems(order, 3, new int[] { 0, -1, -1 }, new int[] { 1, 2,3
        // }, "AFTER PERSIST");
        em.getTransaction().commit();
        order = em.find(Order.class, order.getOrderNumber());
        verifyLineItems(order, 2, new int[] { 0, 1 }, new int[] { 2, 3 }, "AFTER COMMIT (working copy)");
        if (!orderEntityIsolated(em)) {
            order = getOrderFromCache(em, order.getOrderNumber());
            verifyLineItems(order, 2, new int[] { 0, 1 }, new int[] { 2, 3 }, "AFTER COMMIT (shared cache)");
        }
        assertOrderMatchesDatabase(getEMF(), order);
        em.close();
    }

    protected boolean orderEntityIsolated(EntityManager em) {
        return JpaHelper.getEntityManager(em).getServerSession().getClassDescriptorForAlias("Order").isIsolated();
    }

    private void verifyLineItems(Order order, int numLineItems, int[] lineNums, int[] prodIds, String logPrefix) {
        assertNotNull("Null order: " + logPrefix, order);
        assertEquals("Test case failure. Incorrect number of lineNums: " + logPrefix, numLineItems, lineNums.length);
        assertEquals("Test case failure. Incorrect number of prodIds: " + logPrefix, numLineItems, prodIds.length);

        if (logPrefix != null) {
            System.out.println(logPrefix + " ORDER: " + order.getOrderNumber() + " :: item count:" + order.getLineItems().size() + " hashCode=" + System.identityHashCode(order));
            for (LineItem item : order.getLineItems()) {
                System.out.println("\t" + item + " hashCode=" + System.identityHashCode(item));
            }
        }

        ClassDescriptor orderDesc = JpaHelper.getServerSession(getEMF()).getClassDescriptorForAlias("Order");
        assertNotNull("No descriptor for for 'Order'", orderDesc);

        OneToManyMapping lineItemsMapping = (OneToManyMapping) orderDesc.getMappingForAttributeName("lineItems");
        assertNotNull("No mapping for for 'Order.lineItems", lineItemsMapping);

        Object lineItemsObj = lineItemsMapping.getAttributeValueFromObject(order);
        assertNotNull("Null lineItems found in Order", lineItemsObj);

        if (lineItemsObj instanceof IndirectContainer) {
            IndirectContainer lineItemsContainer = (IndirectContainer) lineItemsObj;
            assertTrue("IndirectContainer not instantiated", lineItemsContainer.isInstantiated());
        }

        assertEquals("Incorrect number of line items", numLineItems, order.getLineItems().size());

        for (int index = 0; index < numLineItems; index++) {
            LineItem line = order.getLineItems().get(index);
            assertEquals("Incorrect lineNumber in LineItem at index: " + index, lineNums[index], line.getLineNumber());
            assertEquals("Incorrect ProductId in LineItem at index: " + index, prodIds[index], line.getProductId());
            assertEquals("Incorrect Product in LineItem at index: " + index, prodIds[index], line.getProduct().getId());
        }
    }

    /**
     * Retrieve the Order from the cache. If the entity uses shared caching then
     * get the value from the server session's cache. Otherwise retrieve it form
     * the EM's client session cache.
     * 
     * @param emf
     * @param orderNum
     * @return
     */
    public static Order getOrderFromCache(EntityManager em, String orderNum) {
        UnitOfWorkImpl uow = (UnitOfWorkImpl) JpaHelper.getEntityManager(em).getActiveSession();
        IdentityMapAccessor cache = uow.getParent().getIdentityMapAccessor();

        Vector pk = new Vector(1);
        pk.add(orderNum);
        return (Order) cache.getFromIdentityMap(pk, Order.class);
    }

    private Serializable serialize(Serializable source) throws IOException, ClassNotFoundException {
        byte[] bytes = SerializationHelper.serialize(source);
        return (Serializable) SerializationHelper.deserialize(bytes);
    }

    protected static EntityManagerFactory emf;

    public EntityManagerFactory getEMF() {

        if (emf == null) {
            emf = Persistence.createEntityManagerFactory(getPUName(), getEMFProperties());

        }

        // Since this method is called at the start of each test it will verify
        // the weaving
        if (requiresWeaving()) {
            assertTrue("Order class not woven - does not implement PersistenceEntity", PersistenceEntity.class.isAssignableFrom(Order.class));
            assertTrue("LineItem class not woven - does not implement PersistenceEntity", PersistenceEntity.class.isAssignableFrom(LineItem.class));
            assertTrue("Product class not woven - does not implement PersistenceEntity", PersistenceEntity.class.isAssignableFrom(Product.class));

            ClassDescriptor descriptor = JpaHelper.getServerSession(emf).getClassDescriptorForAlias("Order");
            assertNotNull("Could not find descriptor for 'Order'", descriptor);
            assertTrue("Order descriptor does not use AttributeChangeTracking", descriptor.getObjectChangePolicy() != null
                    && descriptor.getObjectChangePolicy() instanceof AttributeChangeTrackingPolicy);

            descriptor = JpaHelper.getServerSession(emf).getClassDescriptorForAlias("LineItem");
            assertNotNull("Could not find descriptor for 'LineItem'", descriptor);
            assertTrue("LineItem descriptor does not use AttributeChangeTracking", descriptor.getObjectChangePolicy() != null
                    && descriptor.getObjectChangePolicy() instanceof AttributeChangeTrackingPolicy);

            descriptor = JpaHelper.getServerSession(emf).getClassDescriptorForAlias("Product");
            assertNotNull("Could not find descriptor for 'Product'", descriptor);
            assertTrue("Product descriptor does not use AttributeChangeTracking", descriptor.getObjectChangePolicy() != null
                    && descriptor.getObjectChangePolicy() instanceof AttributeChangeTrackingPolicy);
        }

        return emf;
    }

    protected String getPUName() {
        return "persist-order";
    }

    protected Map getEMFProperties() {
        return new HashMap();
    }

    protected boolean requiresWeaving() {
        return true;
    }

    @AfterClass
    public static void closeEMF() {
        if (emf != null && emf.isOpen()) {
            emf.close();
        }

        emf = null;
    }

    @Before
    public void initializeDatabase() {
        EntityManager em = null;

        try {
            em = getEMF().createEntityManager();
            CreateDatabase.createScheamandPopluate(em);
        } finally {
            if (em != null && em.isOpen()) {
                if (em.getTransaction().isActive()) {
                    em.getTransaction().rollback();
                }
                em.close();
            }
        }

        JpaHelper.getServerSession(emf).getIdentityMapAccessor().initializeAllIdentityMaps();
    }
}
