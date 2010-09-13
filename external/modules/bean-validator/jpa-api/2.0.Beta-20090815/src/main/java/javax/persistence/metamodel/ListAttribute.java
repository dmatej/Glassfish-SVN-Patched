// $Id:$
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.metamodel;

/**
 * Instances of the type ListAttribute represent persistent
 * List-valued attributes.
 *
 * @param <X> The type the represented List belongs to
 * @param <E> The element type of the represented List
 */
public interface ListAttribute<X, E>
		extends PluralAttribute<X, java.util.List<E>, E> {
}