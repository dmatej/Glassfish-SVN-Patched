/*******************************************************************************
 * Copyright (c) 1998, 2009 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     dclarke - Example: Maintaining Collection Order (Bug 218321)
 *     			 http://wiki.eclipse.org/EclipseLink/Examples/JPA/Collectionordering
 *     
 *******************************************************************************/
package example;

import java.beans.PropertyChangeEvent;
import java.util.*;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.descriptors.changetracking.ChangeTracker;
import org.eclipse.persistence.expressions.ExpressionBuilder;
import org.eclipse.persistence.indirection.*;
import org.eclipse.persistence.internal.sessions.UnitOfWorkImpl;
import org.eclipse.persistence.mappings.*;
import org.eclipse.persistence.queries.ReportQuery;
import org.eclipse.persistence.sessions.*;

/**
 * This SessionEVentListener provides generic support for updating the mapped
 * index value in an entity within a collection mapping. When registered with an
 * active session it will be called prior to the commit/flush of any UnitOfWork
 * (EntityManager transaction). During this call-back it will process through
 * all new or existing objects managed in the transaction and will update the
 * index values.
 * <p>
 * This functionality is provided in this example for users of EclipseLink's JPA
 * 1.0 implementation. TYhe JPA 2.0 implementation coming in EclipseLink 2.0
 * will provide this support transparently.
 * <p>
 * To configure the use of this functionality a SessionCustomizer is typically
 * used: <code>
 * public class ConfigureCollectionIndexing implements SessionCustomizer {<br>
 * <br>
 * 	public void customize(Session session) throws Exception {<br>
 * 		CollectionIndexSessionListener listener = new CollectionIndexSessionListener();<br>
 * <br>
 * 		listener.addCollection(session, Order.class, "lineItems", "setLineNumber");<br>
 * <br>
 * 		session.getEventManager().addListener(listener);<br>
 * 	}<br>
 * </code>
 * <p>
 * The SessionCustomizer can be configured within the persistence.xml:
 * 
 * <pre>
 * &lt;property name=&quot;eclipselink.session.customizer&quot; value=&quot;model.ConfigureCollectionIndexing&quot;/&gt;
 * </pre>
 * <p>
 * <b>Limitations</b>: This functionality is limited to collection mappings
 * where an int attribute on the target is mapped. The @OnetToMany mapping must
 * be configured with @OrderBy("index-attribute") in order to have the
 * collection ordered when read and maintained when merged from a transaction
 * into the cache. If there is a performance loss due to a large quantity of
 * objects being involved in a transaction it is recommended that the developer
 * add the necessary index maintenance code into the domain model's add/remove
 * methods instead of using this approach.
 * 
 * @author dclarke
 * @since EclipseLink 1.1
 */
public class CollectionIndexSessionListener extends SessionEventAdapter {

    private Map<Class, CollectionIndexer> indexers = new HashMap<Class, CollectionIndexer>();

    private Map<Class, CollectionIndexer> getIndexers() {
        return this.indexers;
    }

    /**
     * Add a CollectionIndexer for the provided class' collection. The session's
     * descriptors and mappings are validated to have the specified collection
     * mapping and the index method on the target class of the collection
     * mapping must exist and take a primitive int as its only parameter.
     * 
     * @param entityClass
     * @param collectionAttribute
     * @param indexMethodName
     *            is the setMethod that takes an int. It will be accessed
     *            reflectively so it does not need to be public
     */
    public void addCollection(Session session, Class entityClass, String collectionAttribute, String indexAttribute) {
        ClassDescriptor descriptor = session.getClassDescriptor(entityClass);

        if (descriptor == null) {
            throw new IllegalArgumentException("No descriptor for class: " + entityClass + " in session: " + session);
        }

        DatabaseMapping dbMapping = descriptor.getMappingForAttributeName(collectionAttribute);
        if (dbMapping == null) {
            throw new IllegalArgumentException("No mapping for attribute: " + collectionAttribute + " in: " + descriptor);
        }
        if (!dbMapping.isCollectionMapping()) {
            throw new IllegalArgumentException("Not a CollectioMapping for attribute: " + collectionAttribute + " in: " + descriptor);
        }

        CollectionMapping colMapping = (CollectionMapping) dbMapping;

        descriptor = session.getClassDescriptor(colMapping.getReferenceClass());
        if (descriptor == null) {
            throw new IllegalArgumentException("No descriptor for class: " + entityClass + " in session: " + session);
        }

        dbMapping = descriptor.getMappingForAttributeName(indexAttribute);
        if (dbMapping == null) {
            throw new IllegalArgumentException("No mapping for index attribute: " + indexAttribute + " in: " + descriptor);
        }
        if (!dbMapping.isDirectToFieldMapping()) {
            throw new IllegalArgumentException("Not a DirectToFieldMapping for attribute: " + indexAttribute + " in: " + descriptor);
        }

        getIndexers().put(entityClass, new CollectionIndexer(colMapping, (DirectToFieldMapping) dbMapping));
    }

    @Override
    public void preCalculateUnitOfWorkChangeSet(SessionEvent event) {
        UnitOfWorkImpl uow = (UnitOfWorkImpl) event.getSource();

        for (Object obj : uow.getCloneMapping().keySet()) {
            CollectionIndexer indexer = getIndexers().get(obj.getClass());

            if (indexer != null && !uow.getDeletedObjects().containsKey(obj)) {
                indexer.index(obj, event.getSession());
            }
        }
    }

    /**
     * This class encapsulates the logic around setting the index values in the
     * collection.
     */
    class CollectionIndexer {

        private CollectionMapping mapping;

        private DirectToFieldMapping indexMapping;

        private CollectionIndexer(CollectionMapping mapping, DirectToFieldMapping indexMapping) {
            this.mapping = mapping;
            this.indexMapping = indexMapping;
        }

        private void index(Object entity, Session session) {
            Object value = mapping.getAttributeValueFromObject(entity);

            if (mapping.usesIndirection()) {
                if (value instanceof IndirectContainer) {
                    IndirectContainer container = (IndirectContainer) value;

                    // Handle case where elements are added to a
                    // non-instantiated list
                    if (!container.isInstantiated() && container instanceof IndirectList && ((IndirectList) container).hasAddedElements()) {
                        indexAddedElements((IndirectList) container, entity, session);
                        return;
                    }

                    value = container.getValueHolder();
                }

                if (value instanceof ValueHolderInterface) {
                    // Make sure the indexing does not cause a lazy relationship
                    // to be loaded
                    if (!((ValueHolderInterface) value).isInstantiated()) {
                        return;
                    }
                    value = ((ValueHolderInterface) value).getValue();
                }
            }

            int index = 0;
            for (Object target : ((Collection) value)) {
                Object originalValue = this.indexMapping.getAttributeValueFromObject(target);
                this.indexMapping.setAttributeValueInObject(target, index);

                if (!this.indexMapping.getDescriptor().getObjectChangePolicy().isDeferredChangeDetectionPolicy()) {
                    ChangeTracker tracker = (ChangeTracker) target;
                    if (tracker._persistence_getPropertyChangeListener() != null) {
                        tracker._persistence_getPropertyChangeListener().propertyChange(new PropertyChangeEvent(target, this.indexMapping.getAttributeName(), originalValue, index));
                    }
                }
                index++;
            }
        }

        /**
         * This method will index new items added to an IndirectList that has
         * not been instantiated. In this case the new items are held in the
         * Indirect List's addedElements collection. To handle this case a query
         * is performed on the database to find the maximum index of the
         * existing collection contents and then the new items are assigned
         * their index starting above the max.
         * 
         * CAUTION: There is the potential for concurrent clients to assign the
         * same index values when inserting new collection elements. To avoid
         * this ensure that the parent class is using optimistic locking. If
         * configured the parent will be forced to update.
         * 
         * @param indirectList
         * @param source
         * @param session
         */
        private void indexAddedElements(IndirectList indirectList, Object source, Session session) {
            // Ensure parent/source is forced to update if supported
            if (session.isUnitOfWork() && mapping.getDescriptor().usesOptimisticLocking()) {
                ((UnitOfWork) session).forceUpdateToVersionField(source, true);
            }

            ExpressionBuilder eb = new ExpressionBuilder();
            ReportQuery rq = new ReportQuery(mapping.getReferenceClass(), eb);
            rq.addMaximum(indexMapping.getAttributeName());
            rq.setSelectionCriteria(eb.get("order").equal(source));
            rq.setShouldReturnSingleValue(true);

            Object maxResult = session.executeQuery(rq);
            int maxIndex = maxResult == null ? -1 : ((Number) maxResult).intValue();

            for (Iterator i = indirectList.getAddedElements().iterator(); i.hasNext();) {
                Object target = i.next();

                this.indexMapping.setAttributeValueInObject(target, ++maxIndex);
            }
        }
    }
}
