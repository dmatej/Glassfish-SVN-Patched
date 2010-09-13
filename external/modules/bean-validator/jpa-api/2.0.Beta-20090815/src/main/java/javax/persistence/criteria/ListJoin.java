// $Id: ListJoin.java 17038 2009-07-08 10:58:24Z epbernard $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.criteria;

import java.util.List;
import javax.persistence.metamodel.ListAttribute;

/**
 * The ListJoin interface is the type of the result of
 * joining to a collection over an association or element
 * collection that has been specified as a java.util.List.
 *
 * @param <Z> The source type of the join
 * @param <E> The element type of the target List
 */
public interface ListJoin<Z, E>
		extends PluralJoin<Z, List<E>, E> {

    /**
     * Return the metamodel representation for the list attribute.
     * @return metamodel type representing the List that is
     *         the target of the join
     */
    ListAttribute<? super Z, E> getModel();

    /**
     * Return an expression that corresponds to the index of
     * the object in the referenced association or element
     * collection.
     * This method must only be invoked upon an object that
     * represents an association or element collection for
     * which an order column has been defined.
     * @return expression denoting the index
     */
    Expression<Integer> index();
}