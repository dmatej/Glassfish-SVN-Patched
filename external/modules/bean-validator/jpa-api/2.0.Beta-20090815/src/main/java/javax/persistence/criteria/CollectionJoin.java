// $Id: CollectionJoin.java 17038 2009-07-08 10:58:24Z epbernard $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.criteria;

import java.util.Collection;
import javax.persistence.metamodel.CollectionAttribute;

/**
 * The CollectionJoin interface is the type of the result of
 * joining to a collection over an association or element
 * collection that has been specified as a java.util.Collection.
 *
 * @param <Z> The source type of the join
 * @param <E> The element type of the target Collection
 */
public interface CollectionJoin<Z, E>
		extends PluralJoin<Z, Collection<E>, E> {

	/**
	 * Return the metamodel representation for the collection
	 * attribute.
	 *
	 * @return metamodel type representing the Collection that is
	 *         the target of the join
	 */
	CollectionAttribute<? super Z, E> getModel();
}
