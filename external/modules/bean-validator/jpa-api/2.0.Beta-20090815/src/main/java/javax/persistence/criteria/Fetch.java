// $Id: Fetch.java 17038 2009-07-08 10:58:24Z epbernard $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.criteria;

import javax.persistence.metamodel.Attribute;

/**
 * Represents a join-fetched association or attribute.
 * @param <Z>
 * @param <X>
 */
public interface Fetch<Z, X> extends FetchParent<Z, X> {

    /**
     * Return the metamodel attribute corresponding to the fetch join.
     * @return metamodel attribute for the join
     */
    Attribute<? super Z, ?> getAttribute();

    /**
     * Return the parent of the fetched item.
     * @return fetch parent
     */
    FetchParent<?, Z> getParent();

    /**
     * Return the join type used in the fetch join.
     * @return join type
     */
    JoinType getJoinType();
}
