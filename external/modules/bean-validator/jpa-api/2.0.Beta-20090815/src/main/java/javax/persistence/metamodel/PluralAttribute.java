// $Id:$
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.metamodel;

/**
 * Instances of the type PluralAttribute represent
 * persistent collection-valued attributes.
 *
 * @param <X> The type the represented collection belongs to
 * @param <C> The type of the represented collection
 * @param <E> The element type of the represented collection
 */
public interface PluralAttribute<X, C, E>
		extends Attribute<X, C>, Bindable<E> {

	public static enum CollectionType {
		COLLECTION, SET, LIST, MAP
	}

	/**
	 * Return the collection type.
	 *
	 * @return collection type
	 */
	CollectionType getCollectionType();

	/**
	 * Return the type representing the element type of the
	 * collection.
	 *
	 * @return element type
	 */
	Type<E> getElementType();
}
