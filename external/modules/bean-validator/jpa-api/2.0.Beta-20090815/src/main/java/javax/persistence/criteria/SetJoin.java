// $Id: SetJoin.java 17038 2009-07-08 10:58:24Z epbernard $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.criteria;

import java.util.Set;
import javax.persistence.metamodel.SetAttribute;

/**
 * The SetJoin interface is the type of the result of
 * joining to a collection over an association or element
 * collection that has been specified as a java.util.Set.
 *
 * @param <Z> The source type of the join
 * @param <E> The element type of the target Set
 */
public interface SetJoin<Z, E> extends PluralJoin<Z, Set<E>, E> {

    /**
     * Return the metamodel representation for the set attribute.
     * @return metamodel type representing the Set that is
     *         the target of the join
     */
    SetAttribute<? super Z, E> getModel();
}
