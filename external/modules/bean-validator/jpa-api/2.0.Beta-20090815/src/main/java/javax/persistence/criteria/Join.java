// $Id: Join.java 17038 2009-07-08 10:58:24Z epbernard $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.criteria;

import javax.persistence.metamodel.Attribute;

/**
 * A join to an entity or embeddable type.
 * @param <Z> The source type of the join
 * @param <X> The target type of the join
 */
public interface Join<Z, X> extends From<Z, X> {

    /**
     * Return the metamodel attribute corresponding to the join.
     * @return metamodel attribute corresponding to the join
     */
    Attribute<? super Z, ?> getAttribute();

    /**
     * Return the parent of the join.
     * @return join parent
     */
    From<?, Z> getParent();

    /**
     * Return the join type.
     * @return join type
     */
    JoinType getJoinType();
}
